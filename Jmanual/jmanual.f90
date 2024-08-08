! Copyright (C) 2022 Juha Lappi
! This is published using MIT licence
! this program can be used  to generate file jmanual.tex which can be used to generate the
! PDF -manual (together with jmain.text)
! The program lso generates the scrip file jexamples.txt which can be used to run all examples in the manual
! The program is using two input files, in adition to the source code file jutilities.f90, j.f90 and jlp.f90. jections.txt and jsections2.txt.
! The manual consists of sections which are defined either in the source files or in the
!  file jections.txt. The file jsections2.txt defines in which order the sections are put into the manual
! and what is the hierarechical status (section, subsection, subsubsection) os each 'raw' section.
! Note the used documentclass does not recognize chapters.
module objmod
	integer,parameter::nobjects=104
	character*20,dimension(nobjects)::objects
	integer,dimension(nobjects)::lobject
	data objects	/ 'Names','Pi','$0','$1','Inf','Tolast','Maxnamed','$Recursion','$Cursor$','$Cursor2$',&
 '$Cursor2$','$','Round','Change%','Imp','LastData','Accepted','Obs','Record','Subrecord', &
 'Duplicate','$Input0$','$Input1$','$Input2$','b','B','di','DI','dg','DG', &
 'bn','BN','CHAR','LIST2','MATRIX','TRANS','LIST','TXT','DATA','FIG',  &
 'REGR','BITMATRIX','REAL','Fakematrix','Black','Red','Green','Blue','Cyan','Violet',&
 'Yellow','Maxlines','Selected','Printinput','Printoutput','$Buffer','Debug','Regf','Resid','Debugconsole',&
 'Arg','Continue','Err','Result','Data','$','Printresult','$$','Terminal','Window',&
 'jlpcoefa','Debugtrans','bgaya','$Cursori$','transa','transb','matrixa','matrixb','reala','realb', &
 'fig','figa','lista','listb','proba','probb','dataa','datab','datac','jlpa',&
 'jlpb','probz','probza','jlpza','ILIST','x','y','z','x1','x2',&
 'x3','y1','y2','y3'&
	/
endmodule 
 
program j_manual
	!use koemod,only: koe2

	use objmod
	use jmod, only: j_nfunctions_,j_noptions_,j_functions,j_options,j_maxarg_,j_minarg_
	integer,parameter ::maxsections=1000
	integer,parameter :: maxmacros=200  !common optio ns
	integer,parameter :: maxfuncs0=100 !per section
	integer,parameter :: maxfuncs=1000 !all together
	integer,parameter::maxhead=2000 !lines per h ea der
	integer,parameter::maxsec=1000 !lines per sectio
	integer,parameter::maxlfunc=10000 !lines per function
	integer,parameter::maxex=10000 !examples
	integer,parameter::maxlex=500 !lines per example
	integer,parameter::maxopt=1000 !maximum number of option sections
	integer::nitem=0
	character*180,dimension(maxlex,maxex)::ex
	integer,dimension(maxlex,maxex)::leex=0
	integer,dimension(maxex)::ninexample=0
	logical verbatim
	integer,dimension(maxex)::fexl,lexl !first and last line in example
	integer,dimension(maxsections)::fex,lex !first and last example in section
	logical,dimension(maxsections):: written=.false.
	character*80,dimension(maxex)::exlabel
	integer,dimension(maxex)::lexlabel
	character*6,dimension(maxex)::exline
	!	character*30,dimension(maxex):: exfile
	integer::nexample=0
	integer,parameter::maxnote=10000 !examples
	integer,parameter::maxlnote=100 !lines per note
	character*500,dimension(maxnote,maxlnote)::note  !note
	character*10,dimension(maxex)::notelabel
 
	integer,dimension(maxsections)::fnote,lnote=0
	integer,dimension(maxnote)::fnotel,lnotel=0
	character*40,dimension(maxex)::exfile
	character*300,dimension(maxsections)::section
	character*20,dimension(maxsections)::sectionlabel
	character*20,dimension(maxmacros)::macrolabel
	character*5,dimension(7)::tabto
	character*50,dimension(maxsections)::sectionindex
	character*300::jatko
	integer,dimension(maxsections)::fopt,lopt=0  !first and last option
	integer,dimension(maxopt)::foptl,loptl=0  !first and last line in option
	!	logical,dimension(maxsections)::subsubsection
	!	logical,dimension(maxsections)::subsection
	integer,dimension(maxsections)::nfuncs=0
	integer,dimension(maxsections)::sectionstart
	integer,dimension(maxsections)::lsection=0 !lenggth of section name
	integer,dimension(maxmacros)::lmacro=0  !number of examples per sectio
	character*1500 line,linec,line2
	character*500,dimension(maxhead,j_nfunctions_)::headers
	integer,dimension(j_nfunctions_)::lheaders=0
 
	character*400,dimension(maxsec,maxsections)::sheaders
	!logical,dimension(maxsec,maxsections)::wasinex=.false.
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
	logical ::inexample,inex2,innote,inheader,insection=.false.,insubsection=.false.
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
	character*60 infile,outfile
	logical ::open20=.false.
	logical::p=.false.
	character*10,dimension(30)::inpuf
	logical ::isopen,isexist

	integer ::linfile
	integer::ninpuf=0
	logical::firstinoption=.false.,inoption0=.false.    !
	integer ::nlblock
 
	logical::firsto=.false. ! is firsrt word in colors object name
integer:: niter

	!	character*60::infile

	do i=1,89
		lobject(i)=len_trim(objects(i))
	enddo
	! logical, dimension(1 0)::logi
	! integer,dimension(10)::inte/
	niter=0
	!nitemize=0
	! inte=0
	! logi=.true.
	! logi(5)=.false.
	! inte=logi
	verbatim=.false.
	
	! write(6,*)'logi',logi
	! write(6,*)'inte',inte

	! stop
	
	!	logical ::frommacro
	write(6,*)j_nfunctions_,' functions available'
	ninpufv=0
	exline=' '
	do j=1,j_nfunctions_
	write(6,*)j,j_functions(j),j_minarg_(j),j_maxarg_(j)
	enddo
	! character*(len=*),parameter :: fucol='\textcolor{VioletRed}{'
	! lfucolp=len(fucol)+1
	! character*(len=*),paramet er :: inpucol='\textcolor{Red}{'
	! linpucolp=len(fucol)+1
	! character*(len=*),parameter ::boxcola='\colorbox{GreenYellow}{'
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
	sectionindex=' '
	ex=' '
	njf=0  !number of !jf  in sections
	njf2=0  !number of !jf in source
	!optline='\begin{tabular}{| m{.10\textwidth} | m{.05\textwidth}|m{.05\textwidth}|p{.70\textwidth}|}'
	optline='\begin{tabular}{ m{.10\textwidth}  p{.05\textwidth}p{.05\textwidth}p{.70\textwidth}}'
	lop=j_lentrim(optline)
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
	open(10,file='jsections2.txt',action='READ',NOSHARED)
100 read(10,'(a)',end=234)line
	write(6,*)'sections2: ',line(1:j_lentrim(line))
	if(line(1:1).eq.'!')goto 100
 
	goto 235
234	write(6,*)nsection,nsections,' etc'
	write(6,*)' '
	write(6,*)'found altogether',nsections,' sections:'
	stop 'here'
235	le=j_lentrim(line)
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
		endif !if(index(line2,line(ifirst(2):ilast(2))//':').le.0)    207
	102 write(6,*)'adress ',line(ifirst(2):ilast(2))//':  NOT FOUND'
		stop 'hyhu'
	endif !if(line(ifirst(1):ilast(1)).eq.';goto')    205
103 	continue
!write(6,*)'<54>',line(ifirst(2):ilast(2))
!	write(6,*)'<33>',line(1:le)
	if(index(line(1:le),';return').gt.0)stop ';return'
 
	if(index(line(1:le),'infile').gt.0)then
		call words(line,1,le,nwords,ifirst,ilast,iss)
		infile=line(ifirst(2):le)
		linfile=le-ifirst(2)+1
		open(2,file=line(ifirst(2):le),action='READ')
		nl=0
		nlsection=0
		ifi=ifi+1
		!	if(ifi.gt.1)call testblock() tested in readinput
		call readinput()
 
		write(6,*)'found ', nsection-nsection0,' sections in ',infile(1:linfile)
		do isec=nsection0+1,nsection
 
 
			lelabel=j_lentrim(sectionlabel(isec))
			lesec=j_lentrim(section(isec))
			write(6,*)isec,sectionstart(isec),sectionlabel(isec)(1:lelabel),' ',section(isec)(1:lesec)
 
		enddo !isec=nsection0+1,nsection    232
 
		if(ninpuf.gt.ninpufv)then
			write(6,*)'the following commands were declared as input programming words to be colorized'
			do ipu=ninpufv+1,ninpuf
				write(6,*)ipu, inpuf(ipu)
 
			enddo !ipu=ninpufv+1,ninpuf    243
 
		endif !if(ninpuf.gt.ninpufv)    241
 
 
		nsection0=nsection
		! write(6,*)'found ',count(funcsinsec),' functions out of ',j_nfunctions_
		! write(6,*)'found ',count(comoptions),' options out of ',j_noptions_
		if(nsection.eq.0)stop 'nsection=0'
		close(2)
		!		isinput=.false.
		goto 100
 
	elseif(index(line(1:le),'outfile').gt.0)then
		
		if(line(le-3:le).eq.'.txt')then
			write(6,*)'opening ',line(ifirst(2):le)
		open(19,file=line(ifirst(2):le),status='REPLACE',action='WRITE')
		inquire(unit=19,opened=isopen)
		!write(6,*)'isopen ',isopen
	
		!inquire(file=line(ifirst(2):le),exist=isexist)
	!	write(6,*)'isexist ',isexist
			call writeincl()
			close(19)
			
			goto 100
		else
		open(20,file=line(ifirst(2):le),status='REPLACE',action='WRITE')
		call writeoutput()
		stop 'here' 
		endif !if(line(le-3:le).eq.'.txt')    265
		
	!	write(6,*)'goto100'
		!	goto 100
		! else
		! write(6,*)'<337writeoutput ',line(1:le)
		! write(6,*)'<429>',nwords,(line(ifirst(ii):ilast(ii))//'/',ii=1,nwords)
		! call writeoutput()
		! goto 100
 
	endif !if(index(line(1:le),'infile').gt.0)    220
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
 
 
 
	99 write(6,*)'line ',nl,' in ',infile(1:linfile), ' first character ',ch,' is not !'
	contains
	subroutine writeincl()
	character*80 exline0
	character*1 eline
	integer lexline
		write(6,*)'writing ',nexample, ' examples as include file jexamples.txt'
		write(19,*)'** this file contains ',nexample,' examples of using Jlp22'
		write(19,'(a)')'** launch Jlp22 in the command promt in the folder of this file '
		write(19,'(a)')'** using: Jlp22 jexamples.txt'
		write(19,'(a)')'** if you want to pause after each example answer to first question with 1'
		write(19,'(a)')'** if not, answer with <return>'
		write(19,'(a)')'** if you want to pause after each figure answer to second question with 1'
		write(19,'(a)')'** if not, answer with <return>'

		eline=','
		
		write(19,'(a)')'exfile=thisfile()'
	write(19,'(a)')"AGAIN=';incl("//'"'//'exfile'//'"'//")'"
	write(19,*)'examples=macro('
	exline0='current,ALL,'
	lexline=12
	idone=0
write(6,'(a)')"current=';incl(exfile,from->current)'"
		do ine=1,nexample
			lef=len_trim(exfile(ine))
			if(ine.eq.nexample)eline=')'
			if(lexline+lef.le.79)then
			
			 exline0(lexline+1:lexline+lef)=exlabel(ine)(1:lexlabel(ine))//eline
			 lexline=lexline+lef+1
			 
			 if(ine.eq.nexample)write(19,*)exline0(1:lexline)
			else
		
			write(19,*)exline0(1:lexline)
			exline0=exlabel(ine)(1:lexlabel(ine))//eline
			lexline=lef+1
			 if(ine.eq.nexample)write(19,*)exline0(1:lexline)
		endif
		enddo
	
	!		write(6,*)ine, exlabel(ine)(1:lexlabel(ine)),' from ',exfile(ine),' line ',exline(ine)
	!		write(19,'(a)')exlabel(ine)(1:lexlabel(ine))//"=';incl(exfile,from->"// &
	!			exlabel(ine)(1:lexlabel(ine))//")'"
	!	enddo !ine=1,nexample    316
	!	write(19,'(a)')"AGAIN=';incl(exfile)'"
	!	write(19,'(a)')"ALL=';incl(exfile,from->ALL)'"
		!		write(19,'(a)')"ask(fcont,q->'continue after each figure(1/0)>')"
		!		write(19,'(a)')';if(wait);pause'
		write(19,'(a)')"ask(wait,q->'pause after each example(1/0)>')"
		write(19,'(a)')"ask(fpause,q->'pause after each figure(1/0)>')"
		write(19,'(a)')"fcont=.not.fpause"
		write(19,'(a)')'**'
		write(19,'(a)')'** if you want to run all examples type: ALL'
		write(19,'(a)')'** if you want to run a specific example type the name of the example'
		write(19,'(a)')'** if you want to run all examples after a specific example'
		write(19,'(a)')'** go to the final section after ;ALL'
write(19,'(a)')'** and put text ;current: before the the name of the example , an type then '
		write(19,'(a)')'** current'
		write(19,'(a)')'** even if running with ALL without pause for figures and examples'
		write(19,'(a)')'** Jlp22 asks some numbers to demonstrate ask() function'
		write(19,'(a)')';return'
	
		write(19,'(a)')' '
		do ine=1,nexample
			lef=len_trim(exfile(ine))
			write(19,'(a)')' '
			write(19,'(a)')';'//exlabel(ine)(1:lexlabel(ine))//':  '
			write(19,'(a)')'**'//exlabel(ine)(lexlabel(ine)+1:j_lentrim(exlabel(ine)))//' (line '//&
				exline(ine)//' file '//exfile(ine)(1:lef)//')'
			do ine2=1,ninexample(ine)
			lee=leex(ine2,ine)
			ibs=index(ex(ine2,ine)(1:lee),'?/?')
			if(ibs.le.0)then
	!				write(19,'(a)')ex(ine2,ine)(1:leex(ine2,ine))
				write(19,'(a)')ex(ine2,ine)(1:lee)
				else
				
				line(1:lee)=ex(ine2,ine)(1:lee)
		!		write(6,*)line(1:lee)
	458			line(ibs:lee-2)='\'//line(ibs+3:lee)
				lee=lee-2
				ibs=index(line(1:lee),'?/?')
			!	write(6,*)line(1:lee)
			!	write(6,*)'ibs',ibs
				if(ibs.gt.0)goto 458
				write(19,'(a)')line(1:lee)
				
			!	write(6,*)ex(ine2,ine)(1:leex(ine2,ine))
			!	write(6,*)line(1:lee),'?'
		!		read(5,*)iiip
				
				endif
			enddo !ine2=1,ninexample(ine)    337
			write(19,'(a)')';if(wait);pause'
			write(19,'(a)')';return'
			write(19,'(a)')' '
 
		enddo !ine=1,nexample    331
 
		write(19,*)' '
		write(19,'(a)')';ALL:'
		!	write(19,'(a)')"ask(fcont,q->'continue after each figure(1/0)>')"
		do ine=1,nexample
			!	write(19,'(a)')' '
			!	write(19,'(a)')';'//exlabel(ine)(1:lexlabel(ine))//':'
			write(19,'(a)')exlabel(ine)(1:lexlabel(ine))
			write(6,'(a)')exlabel(ine)(1:lexlabel(ine))
 
		enddo !ine=1,nexample    350
		write(19,'(a)')';return'
		close(19)

 
	end subroutine
 
	subroutine writeoutput()
		logical isend
		logical doit
		character*1 ::yesno
		isend=.true.
700	 read(10,'(a)',end=999)line;nl=nl+1;le=j_lentrim(line) !read(2,'(a)',end=40)line;nl=nl+1;le=j_lentrim(line)
		if(line(1:1).eq.'!')goto 700
	
		if(le.le.0)goto 700
			write(6,*)line(1:le)
		if(index(line(1:le),';return').gt.0)then
			goto 999
			isend=.false.
		endif !if(index(line(1:le),';return ').gt.0)    370
		! iret=index(line,'inpureturn')
		! if(iret.gt.0)write(6,*)'le ',le,'/'//line(1:le)//'/'
		call words(line,1,le,nwords,ifirst,ilast,iss)  !
		!	if(iret.gt.0)write(6,*)nwords,ifirst(1:nwords),ilast(1:nw ords),'/'//line(ifirst(2):ilast(2))//'/'
 
		do isec=1,nsection
 
			lelabel=j_lentrim(sectionlabel(isec))
			!	if(isec.eq.6)write(6,*)'** ',lelabel,line(ifirst(2):ilast(2)),'?',sectionlabel(isec)(1:lelabel)
			if(sectionlabel(isec)(1:lelabel).eq.line(ifirst(2):ilast(2)))then
				lesec=j_lentrim(section(isec))
				leni=len_trim(sectionindex(isec))
				if(section(isec)(1:5).ne.'LATEX')then
					!	if(isec.eq.54)write(6,*)'isec ',isec,sectionindex(isec),leni
					!if(isec.eq.54) stop 'kdkkd'
					if(line(le:le).eq.'*')then
						if(leni.gt.0)then
						line='\'//line(ifirst(1):ilast(1))//'*{'//&
								section(isec)(1:lesec)//'} \index{'// &
								sectionindex(isec)(1:leni)//'}  % '//line(ifirst(2):ilast(2))
						lei=len_trim(line)
						call final(line,lei,line2,le2,.true.)
						
						write(20,'(a)')line2(1:le2)//' '
 
							! write(20,'(a)')'\'//line(ifirst(1):ilast(1))//'*{'//&
								! section(isec)(1:lesec)//'} \index{'// &
								! sectionindex(isec)(1:leni)//'}  % '//line(ifirst(2):ilast(2))
						else
							!if(leni.gt.0)then
							line='\'//line(ifirst(1):ilast(1))//'*{'//section(isec)(1:lesec)//'} ' // &
								' % '//line(ifirst(2):ilast(2))
							lei=len_trim(line)
							call final(line,lei,line2,le2,.true.)
							write(20,'(a)')line2(1:le2)//' '
							! write(20,'(a)')'\'//line(ifirst(1):ilast(1))//'*{'//section(isec)(1:lesec)//'} ' // &
								! ' % '//line(ifirst(2):ilast(2))
 
						endif !if(leni.gt.0)    390
						line='\addcontentsline{toc}{section}{'//section(isec)(1:lesec)//'}'
					
							lei=len_trim(line)
							call final(line,lei,line2,le2,.true.)
							write(20,'(a)')line2(1:le2)//' '
					!	write(20,'(a)')'	\addcontentsline{toc}{section}{'//section(isec)(1:lesec)//'}'
					else
						if(leni.gt.0)then
						line='\'//line(ifirst(1):ilast(1))//'{'//section(isec)(1:lesec)//'}'//&
									'\index{'//sectionindex(isec)(1:leni)//'}'
									lei=len_trim(line)
								call final(line,lei,line2,le2,.true.)
							write(20,'(a)')line2(1:le2)//' '
								! write(20,'(a)')'\'//line(ifirst(1):ilast(1))//'{'//section(isec)(1:lesec)//'}'//&
									! '\index{'//sectionindex(isec)(1:leni)//'}'
									! ! if(sectionlabel(isec)(1:lelabel).eq.'ROB')then
									! write(20,*)'lesec,leni ',lesec,leni
									! write(20,'(a)')line(ifirst(1):ilast(1))
									! write(20,'(a)')section(isec)(1:lesec)
									! write(20,'(a)')sectionindex(isec)(1:leni)
									! stop
									! endif
	
						
						else
						lei=ilast(1)-ifirst(1)+lesec+4
						call final('\'//line(ifirst(1):ilast(1))//'{'//section(isec)(1:lesec)//'}',lei,&
						line2,le2,.true.)
							write(20,'(a)')line2(1:le2)//' '
				!			write(20,'(a)')'\'//line(ifirst(1):ilast(1))//'{'//section(isec)(1:lesec)//'}'
						endif !if(leni.gt.0)    403
					endif !if(line(le:le).eq.'*')    389
 
 
					write(20,'(a)')'\label{'//sectionlabel(isec)(1:lelabel)//'} '
				endif !if(section(isec)(1:5).ne.'LATEX')    386
				nl2=nl2+2
				write(6,*)'     writing section ',isec, ' ',sectionlabel(isec)(1:lelabel),'  ',lsection(isec)+2,' lines'
				do j=1,lsection(isec)
					lei=j_lentrim(sheaders(j,isec))
					if(sheaders(j,isec)(1:lei).eq.'\begin{verbatim}')verbatim=.true.
					if(sheaders(j,isec)(1:lei).eq.'\end{verbatim}')verbatim=.false.
					doit=.not.verbatim.and.index(sheaders(j,isec)(1:lei),'\url').le.0
			
					call final(sheaders(j,isec),lei,line2,le2,doit)
					
						ibs=index(line2(1:le2),'?/?')
						if(ibs.gt.0)then
				!		write(6,*)'line20:',line2(1:le2)
						558			line2(ibs:le2+12)='\textbackslash '//line2(ibs+3:le2)
				le2=le2+12
				ibs=index(line2(1:le2),'?/?')
			!	write(6,*)'line2:',line2(1:le2)
		!		write(6,*)'ibs',ibs
				if(ibs.gt.0)goto 558
			
				
			!	read(5,*)iiii
						
						
						endif
						
					if(line2(1:le2).ne.'\\')write(20,'(a)')line2(1:le2)//' '
			
					! if(nsi.gt.0)then
						! write(20,'(a)')line2(1:ialout)//sheaders(j,isec)(ialin:lei)
						! write(17,'(a)')'#',line2(1:ialout)//sheaders(j,isec)(ialin:lei)
					! else
						! if(sheaders(j,isec)(1:lei).ne.'\\')write(17,'(a)')sheaders(j,isec)(1:lei)
						! if(sheaders(j,isec)(1:lei).ne.'\\')write(20,'(a)')sheaders(j,isec)(1:lei)
					! endif
				enddo !j=1,lsection(isec)    416
				written(isec)=.true.
				nl2=nl2+lsection(isec)
				goto 700
			endif !if(sectionlabel(isec)(1:lelabel).eq.line(ifirst(2):ilast(2    383
 
		enddo !isec=1,nsection    379
		write(6,*)' '
		write(6,*)'*********',line(ifirst(2):ilast(2)), ' not found in sections'
		write(6,*)' '
		stop 'section not found'
		!		goto 700
999	 write(6,*)' '
		nonw=0
		do isec=1,nsection
			lelabel=j_lentrim(sectionlabel(isec))
			if(.not.written(isec))then
				write(6,*)'**Section ',isec,' ',sectionlabel(isec)(1:lelabel),' not written earlier'
				lesec=j_lentrim(section(isec))
				nonw=nonw+1
1000 format(a,$)
				if(nonw.eq.1)then
					write(6,1000)'are sections not written earlie written now (y/n)'
					read(5,'(a)')yesno
					if(yesno.ne.'y'.and.yesno.ne.'Y')cycle
				else
					if(yesno.ne.'y'.and.yesno.ne.'Y')cycle
				endif !if(nonw.eq.1)    440
				write(20,'(a)')'\section*{'//section(isec)(1:lesec)//'} '
				write(20,'(a)')'EXTRAAAAAAAAAAAAAAA'
				nonw=nonw+1
				do j=1,lsection(isec)
					lei=j_lentrim(sheaders(j,isec))
					if(sheaders(j,isec)(1:lei).ne.'\\')write(20,'(a)')sheaders(j,isec)(1:lei)
				enddo !j=1,lsection(isec)    450
			endif !if(.not.written(isec))    435
		enddo !isec=1,nsection    433
		if(nonw.gt.0.and.(yesno.eq.'y'.or.yesno.eq.'Y'))write(6,*)nonw,' sections written which were not written earlier'
 
 
 
		if(isend)stop 'isend'
		return
 
 
 
 
	end subroutine
 
	subroutine readinput()
	logical nocomment
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
		endif !if(insection)    495
		nsection0=nsection
		insection=.false.
		inexample=.false.;inex2=.false.
		inheader=.false.
		innote=.false.
		isoption=.false.
		inmacro=.false.
		isnote=.false.
		isexample=.false.
		frommacro=.false.
		nl=0
		nlsection=0
1  continue
		if(p)write(6,*)'<76',inoption,nl,line(1:40),frommacro
		if(frommacro)then
			lfrommacro=lfrommacro+1
			write(6,*)'<44frommacro',lfrommacro,lmacro(imacro)
			! write(6,*)'inheader,inexample,inex2,innote'// &
			! 'inlisting,inmacro,inoption,inlatex,intabular',&
			! inheader,inexample,inex2,innote,&
			! inlisting,inmacro,inoption,inlatex,intabular
			if(lfrommacro.gt.lmacro(imacro) )then
				imacro=0
				frommacro=.false.
				goto 1
			endif !if(lfrommacro.gt.lmacro(imacro) )    519
			line=macroheaders(lfrommacro,imacro)
			le=j_lentrim(line)
			nb=nonblank(line,1,le)
			if(nb.gt.1)then
				line(1:le-nb+1)=line(nb:le)
				le=le-nb+1
			endif !if(nb.gt.1)    527
				write(6,*)'FROMMACRO:',line(1:le)
			!	read(5,*)ii
			!	p=.true.
		else
			!17		read(2,'(a1,a)',end=789)ch,line;nl=nl+1;le=j_lentrim(line)
17		read(2,'(a)',end=789)line;nl=nl+1;le=j_lentrim(line)

			nb=nonblank(line,1,le)  !if le=0 nb=1
			
			if(line(nb:nb+2).eq.'!@@'.or.index(line(1:le),'@@').gt.0)then
			write(6,*)nb,'<22>',line(1:le),line(nb:nb+2).eq.'!@@',line(le:le).eq.'@'
	!		read(5,*)ii
			if(line(le:le).eq.'@')then
			imacro=j_isin(line(nb+3:le-1),macrolabel,nmacro)
		!	write(6,*)line(ifirst(1):ilast(1))
			if(imacro.le.0)then
				write(6,*)'line ',nl, ' in ',infile(1:linfile),' ' ,line(nb+3:le-1),'  is not among macros: '
				do ico=1,nmacro
					write(6,*)macrolabel(ico)
				enddo !ico=1,nmacro   1251
				stop 'mcmcm'
			endif !if(imacro.le.0)   1249
			frommacro=.true.
			lfrommacro=0
			write(6,*)'Macro ',line(nb+3:le-1),imacro,' in line ',nl
		!	read(5,*)iii
			goto 1
			endif
		endif !if(line(nb+1:nb+1).eq.'@@')   1247
		!!!!!! ordinary line
		! if(index(line(1:le),'@@').gt.0)then
		! write(6,*)'/',line(1:le),'/'
		! write(6,*)nb,imacro,frommacro, 'line ',nl
	! !	read(5,*)ii
		! endif
			!		if(iper.eq.100)write(6,*)'jdjdj',line(1:le+1)
	
			if(index(line(1:le),'@@').gt.0)then
	write(6,*)'@@675',nl,nb
!	read(5,*)iii
	endif
		
			if(le.eq.0.and.insection)then
				call putsec(' ')
				goto 17
			endif !if(le.eq.0.and.insection)    538
			if(line(nb:nb).ne.'!')goto 17
			nb2=nonblank(line,nb+1,le)
		nocomment=line(nb2+1:nb2+1).ne.'!'.and.line(nb2+1:nb2+1).ne.'*'
			if(line(nb2:nb2).eq.'!'.and.nocomment)goto 17
			
			iter=index(line(1:le),'itemize')
			if(iter.gt.0)then
				ie=index(line(1:iter),'end')
				ib=index(line(1:iter),'begin')
				if(ie.eq.0.and.ib.eq.0)then
 
				elseif(ie.gt.0)then
					niter=niter-1
					if(niter.lt.0)then
						write(6,*)'line ',nl, ' in ',infile(1:linfile), &
							'  more end{itemize} than begin{itemize} ',line(1:le)
						stop 'itemize'
					endif !if(niter.lt.0)    551
 
				elseif(ib.gt.0)then
					niter=niter+1
 
				else
					write(6,*)'line ',nl, ' in ',infile(1:linfile), &
						'  illegal itemize ',line(1:le)
					stop 'itemize'
				endif !if(ie.eq.0.and.ib.eq.0)    547
			endif !if(iter.gt.0)    544
		!	if(iter.gt.0)write(6,*)'ITEMIZE ',niter,line(1:le),niter
			if(nb.lt.le)then
				nb2=nonblank(line,nb+1,le)
				line=line(nb2:le)
				le=le-nb2+1
			endif !if(nb.lt.le)    566
			!read(2,'(a)',end=40)line;nl=nl+1;le=j_lentrim(line)
			goto 790
789 write(6,*)'inputfile ends'
			close(2)
			call testblockend()
			return
790	continue
			if(index(line(1:le),'@@').gt.0)then
	write(6,*)'@@707',nl,nb
	!read(5,*)iii
	endif
	!	write(6,*)'<34fromi2 ',line(1:le)
		!if(nl.eq.48)write(6,*)'inheader0,insection0',inheader,insection
			if(le.ge.3)then
				if(line(le-2:le).eq.'!!!')goto 17
			endif !if(le.ge.3)    581
			!	if(nl.eq.11690)write(6,*)'<33',line(1:le)
			!	if(index(line(1:le),'endsection').gt.0)write(6,*)'<33enndsectioninline ',nl
			!		nb=nonblank(line,1,le)
			if(inmacro)then
				if(line(1:le).eq.'endmacro')then
					inmacro=.false.
 
				else
					!		write(6,*)'putmacro ',line(1:le)
					call putmacro(line(1:le))
				endif !if(line(1:le).eq.'endmacro')    588
				goto 1
			endif !if(inmacro)    587
		endif !if(frommacro)    512
 			if(index(line(1:le),'@@').gt.0)then
	write(6,*)'@@730',nl,nb
	!read(5,*)iii
	endif
 
 
		call words(line,1,le,nwords,ifirst,ilast,iss)  !tarvitaanko
		
		
		
		!	if(nl.eq.11690)write(6,*)'<66',line(ifirst(1):ilast(1)),line(ifirst(1):ilast(1)).eq.'Latex'
		if(p)write(6,*)'<56nwords',nwords
		if(nwords.ge.1)then
			if(line(ifirst(1):ilast(1)).eq.'endlatex')then
				if(.not.inlatex)then
					write(6,*)'line ',nl, ' in ',infile(1:linfile),' endlatex but we are not in latex'
					stop 'nixlatex'
				endif !if(.not.inlatex)    606
				inlatex=.false.
				wasex2=.false.
				goto 1
			endif !if(line(ifirst(1):ilast(1)).eq.'endlatex')    605
		endif !if(nwords.ge.1)    604
		if(inlatex)then
			call colors(firsto,inpuf,ninpuf,line,le,line2,nwords,ifirst,ilast,.false.,isoption,le2,le21,.false.)
			!	call putsec(line(1:max(le,1))) !should be colored
			call putsec(line2(1:max(1,le2)))
			goto 1
		endif !if(inlatex)    615
		!if(nl.eq.48)write(6,*)'<65inheader,insection',inheader,insection
		if(line(ifirst(1):ilast(1)).eq.'Macro')then
			call testblock()
			nmacro=nmacro+1
			macrolabel(nmacro)=line(ifirst(2):ilast(2))
			write(6,*)'line ',nl, ' in ',infile(1:linfile), ' Macro ',nmacro,macrolabel(nmacro)
			inmacro=.true.
			nlblock=nl
			goto 1
		endif !if(line(ifirst(1):ilast(1)).eq.'Macro')    622
 
 
		if(line(ifirst(1):ilast(1)).eq.'Inpuf')then
			! if(index(line,'sit>').gt.0)then
			! write(6,*)'<654>',line(ifirst(1):ilast(1)),line(ifirst(2):ilast(2))
			! elseif(index(line,'stop').gt.0)then
			! write(6,*)'<654>',line(ifirst(1):ilast(1)),line(ifirst(2):ilast(2))
			! stop 'sit'
			! endif
			do ipu=1,ninpuf
				lei=ilast(2)-ifirst(2)+1
				if(inpuf(ninpuf)(1:lei).eq.line(ifirst(2):ilast(2)))goto 756
			enddo !ipu=1,ninpuf    640
			ninpuf=ninpuf+1
			inpuf(ninpuf)=line(ifirst(2):ilast(2))
			!		write(6,*)'line ',nl, ' in ',infile(1:linfile), ' Inpuf ',line(ifirst(2):ilast(2))
		endif !if(line(ifirst(1):ilast(1)).eq.'Inpuf')    633
		! if(index(line(1:le),'itemize').gt.0)then
			! if(index(line(1:le),'begin').gt.0)then
				! nitemize=nitemize+1
			! elseif(index(line(1:le),'end').gt.0)then
				! nitemize=nitemize-1
			! else
				! write(6,*)line(1:le)
				! write(6,*)'line ',nl, ' in ',infile(1:linfile),' itemize without end or begin, do we continue 1/0?'
				! read(5,*)icon
				! if(icon.eq.0)stop
			! endif
		! endif
		 			if(index(line(1:le),'@@').gt.0)then
	write(6,*)'@@823',nl,nb
	!read(5,*)iii
	endif
 
756	if(line(ifirst(1):ilast(1)).eq.'Section'.and.nocomment)then
			if(nwords.lt.3)then
				write(6,*)'line ',nl,' in ',infile(1:linfile), ' section does not have title '
				stop 'no title'
			endif !if(nwords.lt.3)    650
			if(insection)then
			write(6,*)'line ',nl, ' in ',infile(1:linfile),' we are already in a section starting ',&
			sectionstart(nsection)
			write(6,*)' '
			write(6,*)'ERROR'
			stop 'insection'
			endif
			call testblock()
			nlsection=nl
			!		if(nlsection.gt.nlblock)then
			!		write(6,*)'line ', nl ,' Section but block strated at ',nlblock, ' is not closed'
			!			stop 'startsection'
			!		endif
			nsection=nsection+1
			sectionstart(nsection)=nl
			sectionlabel(nsection)=line(ifirst(2):ilast(2))  !can start with limiter
 
			!		write(6,*)'line ',nl,' in ',infile(1:linfile), ' Section ',line(ifirst(2):ilast(2))
			do isec=1,nsection-1
				lelabel0=j_lentrim(sectionlabel(isec))
				if(sectionlabel(isec)(1:lelabel0).eq.line(ifirst(2):ilast(2)))then
					write(6,*)'line ',nl,'**Section ',isec,' has the same name ',line(ifirst(2):ilast(2))
					stop 'duplicate section'
				endif !if(sectionlabel(isec)(1:lelabel0).eq.line(ifirst(2):ilast(    667
			enddo !isec=1,nsection-1    665
			!		if(nl.eq.211)write(6,*)'nwords',nwords,ifirst(1:nwords),ilast(1:nwords),line(1:le)
 
			lefu=0
			iff=index(line(1:le),' if()')
			do nw=3,nwords
				ifu=j_isin(line(ifirst(nw):ilast(nw)),j_functions,j_nfunctions_)
				!	iopt=j_isin(line(ifirst(nw):ilast(nw)),j_options,j_noptions_)
				inpufu=j_isin(line(ifirst(nw):ilast(nw)),inpuf,ninpuf)
				if(ifu.gt.0.and.line(ilast(nw)+1:ilast(nw)+2).eq.'()')then
 
					sectionindex(nsection)(lefu+1:)=line(ifirst(nw):ilast(nw)+2)
					lefu=lefu+ilast(nw)-ifirst(nw)+3
				elseif(inpufu.gt.0)then
					sectionindex(nsection)(lefu+1:)=line(ifirst(nw):ilast(nw))
					lefu=lefu+ilast(nw)-ifirst(nw)+1
				endif !if(ifu.gt.0.and.line(ilast(nw)+1:ilast(nw)+2).eq.'()')    680
				!	if(iff.gt.0)write(6,*)'iff ',iff,nl,nw,line(ifirst(nw):ilast(nw)),ifu,inpufu,line(1:le)
			enddo !nw=3,nwords    676
			!	if(iff.gt.0)stop
			!	if(nl.eq.1234)write(6,*)'nsection ',nsection, line(1:le),nwords,ifirst(1:nwords),ilast(1:nwords),iff,&
			!	sectionindex(nsection)
			!	if(nl.eq.1234)stop 'pi'
			call colors(firsto,inpuf,ninpuf,line,le,line2,&
				nwords,ifirst,ilast,.false.,.false.,le2,le21,.false.)
		!	section(nsection)=line2(ifirst(3):le2)
		section(nsection)=line2(ilast(2)+2:le2)
			!		section(nsection)=line(ifirst(3):le)
			! section cominout Command input and output
			!	character*40,dimension(maxsections)::section
			!character*20,dimension(maxsections)::sectionlabel
 
			!		write(6,*)'line,nsection',nl,nsection,line(ifirst(2):le),'label ',&
			!	sectionlabel(nsection)
			inheader=.true.
			insection=.true.
			nlblock=nl
			goto 1
		endif !756	if(line(ifirst(1):ilast(1)).eq.'Section')    649
		!if(nl.eq.11690)write(6,*)'<66',line(ifirst(1):ilast(1)),line(ifirst(1):ilast(1)).eq.'Latex',insection,&
		!inmacro
		if(line(ifirst(1):ilast(1)).eq.'Subsection'.and.nocomment)then
			inheader=.true.
			insubsection=.true.
		endif !if(line(ifirst(1):ilast(1)).eq.'Subsection')    711
		if(line(ifirst(1):ilast(1)).eq.'endubsection')then
 
			if(.not.inheader.and..not.insection.or..not.insubsection)then
				write(6,*)'line ', nl,' in ',infile(1:linfile), ' !endsubsection even if we are not in subsection'
				stop 'nixsecmac#######################'
			endif !if(.not.inheader.and..not.insection.or..not.insubsection)    717
			inheader=.false.
			insubsection=.false.
			goto 1
		endif !if(line(ifirst(1):ilast(1)).eq.'endubsection')    715
		if(line(ifirst(1):ilast(1)).eq.'Latex'.and.nocomment)then
			!write(6,*)'line ',nl,' in ',infile(1:linfile), ' Latex ',' inoption ',inoption
			nlblock=nl
			inlatex=.true.
			!		if(wasoption.and.inoption)then
			!			call endoption()  !call putsec('\end{table}')
			!			wasoption=.false.
			!		call putsec('\vspace{-1.51em}')
			!	endif
			goto 1
		endif !if(line(ifirst(1):ilast(1)).eq.'Latex')    725
 
		if(line(ifirst(1):ilast(1)).eq.'endsection')then
			if(.not.insection)then
				write(6,*)'line ',nl,' in ',infile(1:linfile),' endsection but we are not in section'
				stop 'nix sec'
			endif !if(.not.insection)    738
			call testblock0()  !if section consists only header it is not necessary to have !endheader
			!		write(6,*)'               !endsection ',nl,' ',sectionlabel(nsection)
			insection=.false.
			inheader=.false.
			goto 1
		endif !if(line(ifirst(1):ilast(1)).eq.'endsection')    737
 
		if(line(ifirst(1):ilast(1)).eq.'endmacro')then
			if(.not.inmacro)then
				write(6,*)'line ',nl,' in ',infile(1:linfile),' endmacro but we are not in macro'
				stop 'nixmacro'
			endif !if(.not.inmacro)    750
			inmacro=.false.
			goto 1
		endif !if(line(ifirst(1):ilast(1)).eq.'endmacro')    749
 
		!if(nl.eq.48)write(6,*)'inheader,insection',inheader,insection
		if(line(ifirst(1):ilast(1)).eq.'endheader')then
 
			if(.not.inheader.and..not.insection)then
				write(6,*)'line ', nl, ' in ',infile(1:linfile), ' !endheader even if we are not in header'
				call inwhat()
				stop 'nixsecmac#######################'
			endif !if(.not.inheader.and..not.insection)    761
			inheader=.false.
			goto 1
		endif !if(line(ifirst(1):ilast(1)).eq.'endheader')    759
 
 
 
		if(line(ifirst(1):ilast(1)).eq.'endlatex')then
			if(.not.inlatex)then
				write(6,*)'line ',nl, ' in ',infile(1:linfile), ' endlatex even if we are not in latex'
				stop 'nix latex###############'
			endif !if(.not.inlatex)    773
			inlatex=.false.
			wasex2=.false.
			goto 1
		endif !if(line(ifirst(1):ilast(1)).eq.'endlatex')    772
 
		iii1=max(ifirst(1)-1,1)
		if(line(ifirst(1):ilast(1)).eq.'Option'.and.nocomment)then
			if(line(iii1:iii1).eq.'+')then
				line(iii1:iii1)=' '
 
			else
				call testblock()
				wasoption=.false.
				inoption=.true.
				inoption0=.false.
				nlblock=nl
				goto 1
			endif !if(line(iii1:iii1).eq.'+')    784
		endif !if(line(ifirst(1):ilast(1)).eq.'Option')    783
		if(line(ifirst(1):ilast(1)).eq.'endoption')then
			if(.not.inoption)then
				write(6,*)'line ',nl, ' in ',infile(1:linfile), ' endoption even if we are not in option'
 
				stop 'enopt###################'
			endif !if(.not.inoption)    797
			!	write(6,*)'line ',nl, ' in ',infile(1:linfile), ' endoption'
			call endoption()
			!	inoption=.false.
			!	wasoption=.false.
			goto 1
		endif !if(line(ifirst(1):ilast(1)).eq.'endoption')    796
 
		isoption=index(line(1:le),'&').gt.0.and.inoption
	748	continue
!	if(nl.eq.979)write(6,*)'TAS',line(1:le),firstinoption,inoption0
		if(isoption)then
			nie=0
			! if(.not.inlatex)then
			if(inoption0)call putsec('\end{changemargin}')
			call putsec('\vspace{0.3cm}')
			call putsec('\hrule')
			call putsec('\vspace{0.3cm}')
			!		call words(line,1,le,nwords,ifirst,ilast,iss)
			!	call colors(firsto,inpuf,ninpuf,line,line2,nwords,ifirst,ilast,.false.,isoption,le2,le21,.false.)
			!		line(1:le2)=line2(1:le2)
			!		le=le2
			!	write(6,*)'line:',	line(1:le)
			do ili=1,le
				if(line(ili:ili).eq.'&')then
					nie=nie+1
					if(nie.eq.1)then
						le2=ili+22
						line2(1:le2)='\noindent '//line(1:ili-1)//' \tabto{3cm} '
						!		write(6,*)'eka:',line2(1:le2)
						iliv=ili
					elseif(nie.eq.2)then
						lis=ili-iliv+13
						line2(le2+1:le2+lis)=line(iliv+1:ili-1)//' \tabto{5cm} '
						le2=le2+lis
						iliv=ili
						!		write(6,*)'toka:',line2(1:le2)
					elseif(nie.eq.3)then
						lis=ili-iliv+13
						line2(le2+1:le2+lis)=line(iliv+1:ili-1)//' \tabto{7cm} '
						le2=le2+lis
						iliv=ili
						!		write(6,*)'kol:',line2(1:le2)
					else
						write(6,*)'line ',nl, ' in ',infile(1:linfile),  ' in ',infile(1:linfile)," should have 3 '&'-characters not",nie
						write(6,*)line(1:le)
						stop 'opt'
					endif !if(nie.eq.1)    827
				endif !if(line(ili:ili).eq.'&')    825
 
			enddo !ili=1,le    824
			if(nie.ne.3)then
				write(6,*)'line ',nl, ' in ',infile(1:linfile),  ' in ',infile(1:linfile), " should have 3 '&'-characters ,not ",nie
				write(6,*)line(1:le)
				write(6,*)line2(1:le2)
				stop 'nix3'
			endif !if(nie.ne.3)    852
		
		!	p=index(line(1:le),' matrix to be generated ').gt.0
			lee=le-iliv
			jatko(1:lee)=line(iliv+1:le)
			! if(nl.eq.2997)then
			if(p)write(6,*)'ine:',iliv,le,lee,le2,line(1:le)
 
			! endif
			line(1:le2-1)=line2(1:le2)
			le=le2-1
		!	if(p)write(6,*)'hui',line(1:le)
			call words(line,1,le,nwords,ifirst,ilast,iss)
			!if(p)		write(6,*)nwords,ifirst(1:nwords),ilast(1:nwords),line(1:le)
			!		stop 'djdj'
			call colors(firsto,inpuf,ninpuf,line,le,line2,nwords,ifirst,ilast,.false.,isoption,le2,le21,.false.)
			call putsec(line2(1:le2))
			call putsec('\begin{changemargin}{3cm}{0cm}')
			line(1:lee)=jatko(1:lee)
			le=lee
			! if(nl.eq.2997)then
			! write(6,*)'ineT:',iliv,le,lee,le2,line(1:le)
			! stop
			! endif
			call words(line,1,le,nwords,ifirst,ilast,iss)
	
			call colors(firsto,inpuf,ninpuf,line,le,line2,nwords,ifirst,ilast,.false.,.false.,le2,le21,.false.)
			! if(p)then
				! write(6,*)'ineTw:',line2(1:le2)
				! !	stop
			! endif !if(nl.eq.2997)    909
			call putsec('\noindent '//line2(1:le2))
			if(p)stop
			! if(ikatk.gt.0)then
			! line=jatko
			! le=lek
			! firstinoption=.true.
			! inoption0=.true.
			! isoption=.false.
			! goto 748
			! else
			!				firstinoption=.true.
			inoption0=.true.
			!			 inoption=.true.
 
			!		 endif
			goto 1
			!		 write(6,*)'tAS:',line(1:le2),inoption0
			! elseif(firstinoption)then
			! !	call putsec('tassa ollaan')
			! call words(line,1,le,nwords,ifirst,ilast,iss)
			! call colors(firsto,inpuf,ninpuf,line,line2,nwords,ifirst,ilast,.false.,isoption,le2,.false.)
			! !	if(firstinoption)then
			! call putsec('\begin{changemargin}{3cm}{0cm}')
			! call putsec('\noindent '//line2(1:le2))
			! firstinoption=.false.
			! inoption0=.true.
			! goto 1
		elseif(inoption0)then
			call words(line,1,le,nwords,ifirst,ilast,iss)
			call colors(firsto,inpuf,ninpuf,line,le,line2,nwords,ifirst,ilast,.false.,isoption,le2,le21,.false.)
			call putsec(line2(1:le2))
			goto 1
		endif !if(isoption)    812
		!	wasoption=.true. !margins changed
 
		!	if(wasoption)then !wasoption means that margin was changed
		!		call putsec('\end{changemargin}')
		!			call endoption()  !call putsec('\end{table}')
		!	call putsec('\vspace{-1.51em}')
		!	endif
 
		! call putsec('\begin{table}[H]')
		!		icopt=j_isin(line(ifirst(1):ilast(1)),macrolabel,nmacro)
		!	wasoption=.true.
		!	goto 1
		!endif
		!if(nl.eq.979)write(6,*)'NYTT',line(1:le),nwords,ifirst(1:nwords),ilast(1:nwords)
 
 
 
 
		if(line(ifirst(1):ilast(1)).eq.'Tabular')then
			call testblock()
			nmerk=ichar(line(ifirst(2):ilast(2)))-48
			!	begintabular=line(ilast(2)+1:le)
			do it=1,nmerk+1
				tabto(it)=line(ifirst(it+2):ilast(it+2))
			enddo !it=1,nmerk+1    968
			intabular=.true.
			ntab=0
			nlblock=nl
			call putsec(' ')
			!	write(6,*)'<888>intabualr',intabular
			goto 1
		endif !if(line(ifirst(1):ilast(1)).eq.'Tabular')    964
 
 
		if(line(ifirst(1):ilast(1)).eq.'endtabular')then
			if(.not.intabular)then
				write(6,*)'line ',nl, ' in ',infile(1:linfile), ' endtabular even if we are not in tabular'
				stop 'FALSE ENDTABULAR'
			endif !if(.not.intabular)    981
			intabular=.false.
			wastabular=.false.
			!		call endtabular()
			goto 1
		endif !if(line(ifirst(1):ilast(1)).eq.'endtabular')    980
		if(p)write(6,*)'<569nwords',nwords,'le',le
		if(intabular)then
			!	write(6,*)'le ',le,line(1:le)
			if(le.le.0)goto 1
			im1=index(line(1:le),'&')
			if(im1.le.0)goto 1
			!	if(im1.gt.0.)then
			nme=1
			do ii=im1+1,le
				if(line(ii:ii).eq.'&')nme=nme+1
			enddo !ii=im1+1,le    998
			if(nme.ne.nmerk)then
				write(6,*)'line ',nl, ' in ',infile(1:linfile),  ' does not have ',nmerk,' & '
				stop 'missing&'
			endif !if(nme.ne.nmerk)   1001
 
			! if(wastabular)then
			! call endtabular()
			! call putsec('\vspace{-1.51em}')
			! endif
 if(index(line(1:le),'@@').gt.0)then
	write(6,*)'@@1172',nl,nb
	!read(5,*)iii
	endif
 
			! call putsec('\begin{table}[H]')
			! call putsec(begintabular(1:j_lentrim(begintabular)))
			nie=0
			le2=0
			do ili=1,le
				if(line(ili:ili).eq.'&')then
					nie=nie+1
					!		write(6,*)'nie ',nie
					if(nie.eq.1)then
						!			call putsec('\noindent ')
						lis=15 !25
						le2=ili+lis
						!	line2(1:le2)='\noindent '//line(1:ili-1)//' \tabto{'//tabto(1)//'} '
						line2(1:le2)=line(1:ili-1)//' \tabto{'//tabto(1)//'} '
						!			write(6,*)'eka:',line2(1:le2)
						iliv=ili
					else
						lis=15+ili-iliv
						line2(le2+1:le2+lis)=line(iliv+1:ili-1)//' \tabto{'//tabto(nie)//'} '
						le2=le2+lis
						iliv=ili
					endif !if(nie.eq.1)   1020
 
				endif !if(line(ili:ili).eq.'&')   1017
 
			enddo !ili=1,le   1016
			lis=le-iliv
			line2(le2+1:le2+lis)=line(iliv+1:le)
			le2=le2+lis
 
			!	endif
			!		write(6,*)line(1:le)
			!		write(6,*)le2,line2(1:le2)
			line(1:le2)=line2(1:le2)
			le=le2
			!	call appsec('\\')
			call words(line,1,le,nwords,ifirst,ilast,iss)
			firsto=.true.
			call colors(firsto,inpuf,ninpuf,line,le,line2,nwords,ifirst,ilast,.false.,.false.,le2,le21,.false.)
			firsto=.false.
175		if(line2(le2:le2).eq.'>')then
				call putsec('\noindent '//line2(1:le2-1)//'\\')
				goto 177
	7899	write(6,*)'premature end of file'
				stop
177			read(2,'(a)',end=7899)line;nl=nl+1;le=j_lentrim(line)

				nb=nonblank(line,1,le)  !if le=0 nb=1
				!		if(iper.eq.100)write(6,*)'jdjdj',line(1:le+1)
				if(line(nb:nb).ne.'!')goto 177
				
				if(nb.lt.le)then
					nb2=nonblank(line,nb+1,le)
					! if(nl.eq.479)write(6,*)'nb,nb2,line(1:le),line(nb2:nb2).eq.!',&
					! nb,nb2,line(1:le),line(nb2:nb2).eq.'!'
					if(line(nb2:nb2).eq.'!')goto 177
					
					line=line(nb2:le)
					le=le-nb2+1
				endif !if(nb.lt.le)   1061
				call words(line,1,le,nwords,ifirst,ilast,iss)
				call colors(firsto,inpuf,ninpuf,line,le,line2,nwords,ifirst,ilast,.false.,.false.,le2,le21,.false.)
				if(line2(le2:le2).eq.'>')goto 175
 
				call putsec('\tabto{'//tabto(nmerk)//'} '//line2(1:le2))
				
				call putsec(' ')
 
			else
				call putsec('\noindent '//line2(1:le2))
				!	if(index(line2(1:le2),' generated ').gt.0)write(6,*)'here&',le2 ,line(1:le2)
				!	write(6,*)'putsec ',le2,line2(1:le2)
				!	iperk=100
				call putsec(' ')
				wastabular=.true.
			endif !175		if(line2(le2:le2).eq.'>')   1052
			!	write(6,*)tabto
			!	stop
			goto 1
		endif !if(intabular)    991
 if(index(line(1:le),'@@').gt.0)then
	write(6,*)'@@1257',nl,nb
	!read(5,*)iii
	endif
 
		if(p)write(6,*)'<587nwords',nwords
		isnote=.false.
		if(line(ifirst(1):ilast(1)).eq.'Note'.and.nocomment)then
		write(6,*)ifirst(1),ilast(1),line(ifirst(1):ilast(1))
			if(line(ilast(1)+1:ilast(1)+1).eq.'#')then
		!		write(6,*)'hellurei ',line(ifirst(1):ilast(1)+1)
				line(ilast(1)+1:ilast(1)+1)=' '
			else
			isnote=.true.
			endif
		endif
		! isnote=line(ifirst(1):ilast(1)).eq.'Note'
		! if(line(ifirst(1):ifirst(1)+3).eq.'Note')write(6,*)'isnote ,line(ifirst(1):ilast(1)).eq.Note#',isnote,&
		! line(ifirst(1):ilast(1)).eq.'Note#'
		! if(line(ifirst(1):ilast(1)).eq.'Note#')line(ilast(1):ilast(1))=' '
		!	if(nl.eq.1406)write(6,*)'insection,innote,inexample,inex2,inoption,inlatex,isnote',&
		!		insection,innote,inexample,inex2,inoption,inlatex,isnote
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
 
			!call colors(firsto,inpuf,ninpuf,line,ifirst(1)-1,line2,nwords,ifirst,ilast,.false.,isoption,le2,.false.)
			call colors(firsto,inpuf,ninpuf,line,le,line2,nwords,ifirst,ilast,.false.,.false.,le2,le21,.false.)
			call putsec(line2(1:le2))
			if(p)write(6,*)'<577le2',nwords
			nlblock=nl
			goto 1
		endif !if(isnote)   1090
		if(nl.eq.14707)write(6,*)'yysys&',ifirst(1),ilast(1),line(ifirst(1):ilast(1)).eq.'endnote'
		if(nl.eq.14707)write(6,*)'****',line,'*',line(ifirst(1):ilast(1)),line(ifirst(1):ilast(1)).eq.'endnote'
		if(line(ifirst(1):ilast(1)).eq.'endnote')then
			if(.not.innote)then
				write(6,*)'line ', nl, ' in ',infile(1:linfile),  'endnote but we are not in note'
				stop 'nixnote'
			endif !if(.not.innote)   1114
			call putsec('\end{note}')
			innote=.false.
			if(nl.eq.14707)write(6,*)'jjhdj',inheader
			goto 1
		endif !if(line(ifirst(1):ilast(1)).eq.'endnote')   1113
 
 	if(index(line(1:le),'@@').gt.0)then
	write(6,*)'@@1313',nl,nb
	!read(5,*)iii
	endif
 
 
		islisting=line(ifirst(1):ilast(1)).eq.'Listing'
		if(islisting)then
			call testblock2()
			call putsec('\color{Green}')
			call putsec('\begin{verbatim}')
			inlisting=.true.
			nlblock=nl
			goto 1
		endif !if(islisting)   1128
		if(line(ifirst(1):ilast(1)).eq.'endlisting')then
			if(.not.inlisting)then
				write(6,*)'line ',nl, ' in ',infile(1:linfile),  'endlisting even if we are not in listing started with Listing'
				stop 'nixlistin'
			endif !if(.not.inlisting)   1137
			call putsec('\end{verbatim}')
			call putsec('\color{Black}')
			nexi=0
			inlisting=.false.
			goto 1
		endif !if(line(ifirst(1):ilast(1)).eq.'endlisting')   1136
		if(inlisting)then
			call putsec(line(1:le))
			goto 1
		endif !if(inlisting)   1147
 
 
		isexample=line(ifirst(1):ilast(1)).eq.'Ex'
		if(isexample)then
			if(nwords.eq.1)then
				write(6,*)'line ',nl, ' in ',infile(1:linfile), ' Ex needs label'
				stop 'nolabel_Ex'
 
			endif !if(nwords.eq.1)   1155
			call testblock()
 
			labe=line(ifirst(2):ilast(2))
			lelabe=ilast(2)-ifirst(2)+1
			nexample=nexample+1
			exlabel(nexample)=line(ifirst(2):le)
			exfile(nexample)=infile(1:linfile)
 
			write(exline(nexample),'(i6)')nl
			lexlabel(nexample)=lelabe
			!write(6,*)'<777',nexample,exlabel(nexample),exfile(nexample),nl
			ifue=j_isin(line(ifirst(2):ilast(2)),j_functions,j_nfunctions_)
 
			if(ifue.gt.0)then
				write(6,*)'line ',nl, ' in ',infile(1:linfile), &
					'  Ex-label is same as function ',line(ifirst(2):le)
				stop 'illegal exlabel'
 
			endif !if(ifue.gt.0)   1173
			! if(index(line(ifirst(2):le),'ilist').gt.0)then
			! write(6,*)'ifue ',ifue,'/'//line(ifirst(2):le)//'/',j_nfunctions_
			! write(6,*)'line ',nl, ' in ',infile(1:linfile), &exfile(nexample)
			! '  Ex-label is same as function ',line(ifirst(2):le)
			! stop 'illegal exl??abel'
			! endif
			!		write(6,*)'                 ',exlabel(nexample)(1:lelabe),':',exlabel(nexample)(lelabe+1:le)
			call putsec('\singlespacing')
			line2='\begin{example}['//labe(1:lelabe)//']'//line(ifirst(3):le)//'\\'
			le=j_lentrim(line2)
			call words(line2,1,le,nwords,ifirst,ilast,iss)  !tarvitaanko
 
			call colors(firsto,inpuf,ninpuf,line2,le,line,nwords,ifirst,ilast,.false.,.false.,le2,le21,.false.)
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
		endif !if(isexample)   1154
 
		if(line(ifirst(1):ilast(1)).eq.'endex')then
			if(.not.inexample)then
				write(6,*)'line ',nl, ' in ',infile(1:linfile),  ' endex but we are not in ex started with !Ex'
				stop 'endex ENDEX'
			endif !if(.not.inexample)   1204
			if(inlisting)then
				write(6,*)'line ',nl, ' in ',infile(1:linfile),  ' in ',infile(1:linfile),' endex but listing is not ended'
				stop 'inlisrt ####'
			endif !if(inlisting)   1208
			inexample=.false.
			!	call putsec('}')
	!		call putsec('\end{exfont}')
			call putsec('\end{example}')
			call putsec('\vspace{-7mm} \rule{5cm}{0.1pt}')
				call putsec('\onehalfspacing')
			goto 1
		endif !if(line(ifirst(1):ilast(1)).eq.'endex')   1203
 
		isex2=line(ifirst(1):ilast(1)).eq.'Ex2'
		if(isex2)then
			call testblock()
			!	if(wasex2)call putsec('\vspace{-1.51em}')
			!	call putsec('\begin{table}[H]') !%    '//line(ifirst(2):le)//']')
			!	call putsec('\label{'//line(ifirst(2):ilast(2))//'}')
			!	call putsec('\begin{tabularx}{\textwidth}{ c  X }')
			call putsec('\\')
			nlblock=nl
			inex2=.true.
			goto 1
		endif !if(isex2)   1219
 
		if(line(ifirst(1):ilast(1)).eq.'endex2')then
			if(.not.inex2)then
				write(6,*)'line ',nl, ' in ',infile(1:linfile),  ' endex2 but we are not in ex2'
				stop 'jdjjd'
			endif !if(.not.inex2)   1232
			inex2=.false.
			wasex2=.true.
			call endex2()
			goto 1
		endif !if(line(ifirst(1):ilast(1)).eq.'endex2')   1231
		! if(inex2)then
		! if(wasex2)call putsec('\vspace{-1.51em}')
 
		! endif
 


 
		if(inlisting)then  !listing can be within example
			call putsec(line(1:max(1,le)))
			goto 1
		endif !if(inlisting)   1263
 
		if(inexample)then
			saa=inexample.and.line(nb:nb).eq.'/'
			!		call putsec(line(nb:le))  ???+
			!if(saa)write(6,*)'<888sanlline',nl,line(1:le)
			!	write(6,*)'<567line',inheader,nl,line(1:le)
			ninexample(nexample)=ninexample(nexample)+1
			ex(ninexample(nexample),nexample)=line(1:max(le,1))
			leex(ninexample(nexample),nexample)=max(le,1)
			if(le.le.0)goto 1
		!	if(nexi.gt.0)call appsec('\\')
			!	subroutine colors(firsto,inpuf,ninpuf,line,ial0,line2,nwords,ifirst,ilast,tabs,isopt,le2,le21,.false.)
			call colors(firsto,inpuf,ninpuf,line,le,line2,nwords,ifirst,ilast,.false.,.false.,le2,le21,.true.)
		
			if(nexi.eq.0)then
			call putsec('\noindent '//line2(1:le21)//'\\')
			else
				call putsec(line2(1:le21)//'\\')
			endif
			if(le21.lt.le2)call putsec('\tabto{3cm}'//line2(le21+1:le2)//'\\')
		!	write(6,*)'\tabto{3cm}'//line2(le21+1:le2)//'\\'
			
						! call putsec(line2(1:le21)//'\\')
						! call putsec('\tabto{3cm}'//line2(le21+1:le2)//'\\')
			
			! else
			! call putsec(line2(1:le2)//'\\')
	
	!		endif
					nexi=nexi+1
			goto 1
		endif !if(inexample)   1268
 
		if(inheader.or.inoption.or.inex2.or.innote)then
			call colors(firsto,inpuf,ninpuf,line,le,line2,nwords,ifirst,ilast,.false.,.false.,le2,le21,.false.)
			call putsec(line2(1:le2))
			if(inex2)call putsec('\\')
		!	write(17,*)'here le2 ',le2 ,line2(1:le2)
			goto 1
		endif !if(inheader.or.inoption.or.inex2.or.innote)   1285
 
 
		goto 1  !there can be empty lines
		write(6,*)'*line*',nl, ' in ',infile(1:linfile), ':',ch,line(1:le)
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
		enddo !j=ifirst(2),le   1316
		le=i
		do i=1,nwords-1
			ifirst(i)=ifirst(i+1)-ialf
			ilast(i)=ilast(i+1)-ialf
		enddo !i=1,nwords-1   1321
		nwords=nwords-1
 
	end subroutine
	subroutine inwhat()
		write(6,*)'inheader ',inheader, 'inexample ',inexample,' inex2 ',inex2, &
			' innote ',innote, 'inmacro ',inmacro,' inoption ',' inlatex ',inlatex,&
			' intabular ',intabular,' insection: ',insection
	end subroutine
 
	subroutine testblock()
 
		if(inheader.or.inexample.or.inex2.or.innote&
				.or.inmacro.or.inoption.or.inlatex.or.intabular)then
			write(6,*)'line ',nl, ' in ',infile(1:linfile), ' cannot start new block before closing the previous block'
			write(6,*)'inheader ',inheader, 'inexample ',inexample,' inex2 ',inex2, &
				' innote ',innote, 'inmacro ',inmacro,' inoption ',' inlatex ',inlatex,&
				' intabular ',intabular
			write(6,*)'the current block started at line ',nlblock
			write(6,*)'the currentsection started at line ',nlsection
			stop 'testb'
		endif !if(inheader.or.inexample.or.inex2.or.inn   1336
		if(.not.insection.and.line(ifirst(1):ilast(1)).ne.'Section' &
				.and.line(ifirst(1):ilast(1)).ne.'Macro')then
			write(6,*)'line ',nl, ' in ',infile(1:linfile), ' cannot start new block if not in section',line(ifirst(1):ilast(1))
			stop 'testbl2'
		endif !if(.not.insection.and.line(ifirst(1):ilast(1)).ne.'Sectio   1346
		if(niter.gt.0)then
			write(6,*)'line ',nl, ' in ',infile(1:linfile), &
				' cannot start new block with ',niter, ' unended begin{itemize}:',line(ifirst(1):ilast(1))
			stop 'itemize'
		endif !if(niter.gt.0)   1351
 
	end subroutine
 
	subroutine testblockend()
		if(inheader.or.inexample.or.inex2.or.innote&
				.or.inmacro.or.inoption.or.inlatex.or.intabular.or.insection)then
			write(6,*)'EOF when unclosed block'
			write(6,*)'the current block started at line ',nlblock
			write(6,*)'the current section started at line ',nlsection
 
			stop 'tesble'
		endif !if(inheader.or.inexample.or.inex2.or.inn   1360
	end subroutine
 
 
 
	subroutine testblock2() !when entering listing listing can be in exaple or in header
		if(inex2.or.innote&
				.or.inmacro.or.inoption.or.inlatex.or.intabular)then
			write(6,*)'line ',nl, ' in ',infile(1:linfile), ' cannot start new block before closing the previous block'
			write(6,*)'inex2.or.innote&.or.inmacro.or.inoption.or.inlatex.or.intabular',&
				inex2,innote,inmacro,inoption,inlatex,intabular
			write(6,*)'the current block started at line ',nlblock
			write(6,*)'the current section started at line ',nlsection
			stop 'djjd'
		endif !if(inex2.or.inn   1373
 
	end subroutine
 
	subroutine testblock0()  !inheader missing
		if(inexample.or.inex2.or.innote.or.&
				inlisting.or.inmacro.or.inoption.or.inlatex.or.intabular)then
			write(6,*)'line ',nl,' in ',infile(1:linfile),' cannot close section before closing the previous block'
			write(6,*)'inexample,inex2,innote,&
				inlisting,inmacro,inoption,inlatex,intabular',&
				inexample,inex2,innote,&
				inlisting,inmacro,inoption,inlatex,intabular
			write(6,*)'the current block started at line ',nlblock
			write(6,*)'the current section started at line ',nlsection
			stop 'dkdk'
		endif !if(inexample.or.inex2.or.innote.   1386
 
	end subroutine
 
 
	subroutine endoption()
		!	call putsec('\\ \cline{1-4}')
		!	call putsec('\end{tabular}')
		!call putsec('\end{table}')
		!	write(6,*)'<5444 ',line(1:le)
		inoption=.false.
		if(inoption0)call putsec('\end {changemargin}')
		call putsec('\hrule')
		call putsec('\vspace{0.2cm}')
		wasoption=.false.
		inoption0=.false.
		!firstinoption=.false.
	end subroutine
 
	subroutine endtabular()
		!	call putsec('\\ \cline{1-4}')
		call putsec('\end{tabular}')
		call putsec('\end{table}')
	end subroutine
 
	subroutine endex2()
		!	call putsec('\\ \cline{1-4}')
		!	call putsec('\end{tabularx}')
		!	call putsec('\end{table}')
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
			enddo !isec=1,nmacro   1434
 
		endif !if(lmacro(nmacro).ge.500)   1430
		lmacro(nmacro)=lmacro(nmacro)+1
		macroheaders(lmacro(nmacro),nmacro)=inpu
		!write(6,*)'<nsec,lsec,inpu',nsection,lsection(nsection),inpu
	end subroutine
 
	subroutine printsec(isec)
		write(6,*)sectionlabel(isec),' lines ',lsection(isec)
		do il=1,lsection(isec)
			write(6,*)il,':',sheaders(il,isec)(1:j_lentrim(sheaders(il,isec)))
		enddo !il=1,lsection(isec)   1446
 
	end subroutine
 
	subroutine putsec(inpu)
		character*(*)inpu
		if(index(inpu,'\textcolor{teal}{textcolor{teal}{Pi}').gt.0)stop
		if(lsection(nsection).ge.1000)then
			write(6,*)'there is too long section ',nsection,' which started at line ',nlsection
 
			write(6,*)'we are now at line',nl, ' in ',infile(1:linfile), ':',line(1:le)
			write(6,*)'Sections and their lengths'
			do isec=1,nsection
				write(6,*)sectionlabel(isec),lsection(isec)
			enddo !isec=1,nsection   1460
			write(6,*)'current section lines'
			call printsec(nsection)
 
		endif !if(lsection(nsection).ge.1000)   1455
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
		endif !if(lsection(nsection).le.0)   1476
		lei=j_lentrim(sheaders(lsection(nsection),nsection))
		sheaders(lsection(nsection),nsection)(lei+1:)=inpu
	end subroutine
end program
 
!! subroutines which are not contained
subroutine colors(firsto,inpuf,ninpuf,line,le,line2,nwords,ifirst,ilast,tabs,isopt,le2,le21,isex)
 
	use jmod, only: j_nfunctions_,j_noptions_,j_functions,j_options
	use objmod
	
	character(len=*),parameter :: fucol='\textcolor{VioletRed}{'
	integer,parameter:: lfucolp=len(fucol)+1
	character(len=*),parameter :: inpucol='\textcolor{Red}{'
	integer,parameter:: linpucolp=len(inpucol)+1
	character(len=*),parameter ::boxcol='\colorbox{GreenYellow}{'
	integer,parameter:: lboxvolp=len(boxcol)+1
	character(len=*),parameter :: optcol='\textcolor{blue}{'
	integer,parameter:: loptcolp=len(optcol)+1
	character(len=*),parameter :: jlpcol='\textbf{Jlp22}'
	integer,parameter:: ljlpcol=len(jlpcol)
	character(len=*),parameter :: gnuplotcol='\textcolor{teal}{gnuplot}'
	integer,parameter:: lgnuplotcol=len(gnuplotcol)
	character(len=*),parameter :: jcol='\textbf{J}'
	integer,parameter:: ljcol=len(jcol)
	character(len=*),parameter :: comcol='\textcolor{green}{'
	integer,parameter:: lcomcol=len(comcol)
	character(len=*),parameter :: objcol='\textcolor{teal}{'
	integer,parameter:: lobjcol=len(objcol)
 
	! tab=9
	!initial secttion is dropped
 
	character*(*) line,line2
	integer ::nwords,ifirst(1:*),ilast(1:*)
	logical isex
	logical tabs,got,isopt,wasblue,ischar,iscom
	character*4 ctabs
	real::left=1.2
	real::tab=0.5
	logical:: perk,p=.false.
	character*10,dimension(10)::inpuf
	logical ::firsto
	logical isfu
	!	lfu=len('\textcolor{VioletRed}{')+1
	!	lbox=len('\colorbox{GreenYellow}{')+1
	!	lvar=len('\textcolor{
!	p=index(line(1:le),'theta').gt.0
	!	stop
	!	le=j_lentrim(line)
	! perk=index(line(index(line(1:le),line(1:1).eq.'?'
	if(p)then
		write(6,*)line(1:le)
		do iw=1,nwords
			write(6,*)'<w>',iw,line(ifirst(iw):ilast(iw)),ifirst(iw)
		enddo !iw=1,nwords   1525
	endif !if(p)   1523
	! if(perk)write(6,*)'<<1>',line(1:le)
	!	perk=inexample.and.line(1:1).eq.'/')  !index(line(1:le),'#').gt.0
	!	if(perk)write(6,*)'<666nl,line',nl,le,'/',line(1:le),'/'
	!	write(6,*)'<5>',line(1:le)¤
	!	write(6,*)nwords,('/'//line(ifirst(iw):ilast(iw))//'/',iw=1,nwords)
	if(nwords.eq.0)then
		line2=line
		return
	endif !if(nwords.eq.0)   1534
	line2=' '
	! if(ial0.gt.0)then
	! line2(1:ial0)=line(1:ial0)
	! le2=ial0
	! j=ial0
	! else
	le2=0
 
	j=0
	!	endif
! fi=draw(func->(transa(f)),x->I,xrange->(0,1000),color->Orange,width->2,continue->,show-
! 0)
! da=newdata(X,Y,e,extra->(Regf,Resid),read->(I,P,er))
! stat()
! fi=plotyx(P,I,append->,show->0,continue->fcont)
! 48
!Y=A*Pmax*X/.(A*X+Pmax)-Res+e !rectangular hyperbola used often for photosynthesis	
 
	ntab=0
	got=.false.
 
	nw=1
	iscom=.false.
	
	if(isex)then
		if(le.gt.80)then
				limi=0.8*le
			write(6,*)limi,le,line(1:le)
			do ile1=le,1,-1
			
				if((line(ile1:ile1).eq.','.or.line(ile1-2:ile1-1).eq.'->'.or.line(ile1:ile1).eq.' '.or.&
				line(ile1:ile1).eq.';'.or.line(ile1:ile1).eq.'='.or.line(ile1:ile1).eq.'+').and.ile1.lt.limi)exit
					
			enddo
			write(6,*)'le,ile1',le,ile1
			write(6,*)line(1:ile1)
		else
		ile1=0
		endif
		nb=nonblank(line,1,le)
		icom=index(line(1:le),'!')
		if(line(nb:nb).eq.'*')icom=nb
		icom1=max(icom-1,1)
		if(line(icom1:icom1).eq."'")icom=0  !ignore '!
		iscom=icom.gt.0
	endif !if(icom.le.0.or.icom.eq.le)   1557

	!p=icom.lt.le
	if(p)write(6,*)'<7773477icom',icom,le,line(1:le)
	ischar=.false.
	!	perk=.false. !index(line(1:le),'$').gt.0
	if(p)then
		write(6,*)'<perk>',line(1:le)
		do iw=1,nwords
			write(6,*)'<w>',iw,line(ifirst(iw):ilast(iw)),ifirst(iw),ilast(iw)
		enddo !iw=1,nwords   1572
	endif !if(p)   1570
	!	if(perk)write(6,*)'<5555>',line(1:le)
	! if(line(1:1).eq.'!')then
	! line2='\colorbox{GreenYellow}{'//line(2:le)//'}'
	! le2=len_trim(line2)
	! return
	! endif
	le21=0
	ile1=0
	if(p)write(6,*)'le,icom',le,icom
100 j=j+1
	if(j.eq.ile1+1)le21=le2
	if(p)write(6,*)'<5line2',le2,'/',line2(1:le2),'/'
	if(p)write(6,*)'<j,nw,ifirst(nw),ch',j,nw,ifirst(nw),'/',line(j:j),'/'
 
	if(nw.le.nwords.and.j.gt.ifirst(nw))then
		write(6,*)'<111>colors, j too big,j,nw,ifirst(nw) ',j,nw,ifirst(nw)
		!	write(6,*)'<77>ial0',ial0,line(1:ial0)
		write(6,*)'<line>',le,line(1:le)
		write(6,*)'<line2>',le2,line2(1:le2)
		do iw=1,nwords
			write(6,*)'<w>',iw,line(ifirst(iw):ilast(iw)),ifirst(iw),ilast(iw)
		enddo !iw=1,nwords   1592
		! endif
		! if(perk)write(6,*)'<<1>',line(1:le)
 
		stop 'colors3'
	endif !if(nw.le.nwords.and.j.gt.ifirst(nw))   1587
	!if(perk)write(6,*)'<48>j,line,nw,ifirst,ilast',j,line(j:j),nw,ifirst(nw),ilast(nw)
	!	if(perk)write(6,*)'<57>,j
	if(j.gt.le.and.p)write(6,*)'<perkout ',line2(1:le2),'icom ',icom
	if(j.gt.le.and.p)stop 'p'
	if(j.gt.le)then
		! if(icom.lt.le)then
			! le2=le2+1
			! line2(le2:le2)='}'
		! endif !if(icom.lt.le)   1605
		if(p)stop
		return
 
	endif !if(j.gt.le)   1604
	! if(line(j:j+1).eq.'\\')then
	! line2(le2+1:le2+2)='\\'
	! le2=le2+2
	! j=j+1
	! goto 100
	! endif
	if(p)write(6,*)'<56000jai'
	 if(line(j:j).eq.'\'.and.line(j+1:j+1).ne.'\')then
	 line2(le2+1:le2+1)='\'
	 le2=le2+1
!	 j=j+1
	goto 100
	 endif
	! if(line(j:
	! if(line(j:j+2).eq.'?/?')then
			! write(6,*)'line20:',line2(1:le2)
		 ! line2(le2+1:le2+15)='\textbackslash '
		 
		! le2=le2+15
			
		! j=j+2
		! write(6,*)'line2:',line2(1:le2)
		! write(6,*)'line:',line(1:j)
		! read(5,*)iiiii
		
		! goto 100
	
	! endif
	 
	if(line(j:j+4).eq.'Jlp22')then
		line2(le2+1:le2+ljlpcol)=jlpcol
		!	 write(6,*)'<23>J',j
		le2=le2+ljlpcol
		nw=nw+1
	 j=j+4
		goto 100
	endif !if(line(j:j).eq.'J'.and.(j.eq.1.or. line(max(j-1,1):max(j-   1625
	
	if(line(j:j+6).eq.'gnuplot')then
	
		line2(le2+1:le2+lgnuplotcol)=gnuplotcol
		!	 write(6,*)'<23>J',j
		le2=le2+lgnuplotcol
		nw=nw+1
	 j=j+6
		goto 100
	endif !if(line(j:j).eq.'J'.and.(j.eq.1.or. line(max(j-1,1):max(j-   1625
	
	if(line(j:j+1).eq.'J '.or.line(j:j+1).eq.'J,'.or.(line(j:j+1).eq.'J.'.and.index(line(j+2:le),'Lappi').le.0))then
		line2(le2+1:le2+ljcol)=jcol
		!	 write(6,*)'<23>J',j
		le2=le2+ljcol
		nw=nw+1
	! j=j+4
		goto 100
	endif !if(line(j:j).eq.'J'.and.(j.eq.1.or. line(max(j-1,1):max(j-   1625
	
	! if(line(j:j).eq.'%')then
 
		! line2(le2+1:le2+2)='\%'
		! le2=le2+2
		! goto 100
	! endif !if(line(j:j).eq.'%')   1634
	
	 ! if(line(j:j).eq.'\')then
	 ! line(le2+1:le2+14)= '\textbackslash'
 
	 ! le2=le2+14
	! goto 100
	! endif
	! if(line(j:j).eq.'$')then
 
		! line2(le2+1:le2+2)='\$'
		! le2=le2+2
		! goto 100
	! endif !if(line(j:j).eq.'$')   1652
	 ! if(line(j:j).eq.'~')then
		! ! !%le2=le2+1
		 ! ischar=.not.ischar
		 ! line2(le2+1:le2+6)='$\sim$'
		 ! le2=le2+6
		 ! goto 100
	! endif !if(line(j:j).eq.'~')   1658
	 ! if(line(j:j).eq.'/')then
		! ! le2=le2+1
 
		! line2(le2+1:le2+15)='\textbackslash '   !'$\backslash$'
		! le2=le2+15  !12
		! j=j+1
		! goto 100
	! endif !if(line(j:j).eq.'/'.and.line(j+1:j+1).eq.'\')   1665
	! if(line(j:j).eq.'#')then
		! !%le2=le2+1
		! !	write(6,*)'<44#in color>',nl,line(1:le)
		! line2(le2+1:le2+2)='\#'
		! le2=le2+2
		! goto 100
	! endif !if(line(j:j).eq.'#')   1673
	
		! if(line(j:j).eq.'_')then
		! ji1=max(j-1,1)
		! if(line(ji1:ji1).ne.'\'.or.j.eq.1)then
		! !%le2=le2+1
		! !	write(6,*)'<44#in color>',nl,line(1:le)
		! line2(le2+1:le2+2)='\#'
		! le2=le2+2
		! goto 100
		! endif
	! endif !if(line(j:j).eq.'#')   1673
	! if(line(j:j).eq.'_')then
	! ! !%le2=le2+1
	! ! !	write(6,*)'<44#in color>',nl,line(1:le)
	! line2(le2+1:le2+2)='\_'
	! le2=le2+2
	! goto 100
	! endif
	if(p)write(6,*)'<56jui'
	if(.not.got)then
		if(tabs.and.ichar(line(j:j)).eq.9)then
			ntab=ntab+1
		endif !if(tabs.and.ichar(line(j:j)).eq.9)   1689
		if(ntab.gt.0.and.ichar(line(j:j)).ne.9)then
			write(ctabs,'(f4.2)')ntab*tab
			line2(1:12)='\hspace{'//ctabs//'}'
			le2=12
			got=.true.
		endif !if(ntab.gt.0.and.ichar(line(j:j)).ne.9)   1692
	endif !if(.not.got)   1688
	!	if(j.lt.ifirst(1))goto 100
	! if(nw.eq.1.and.ifirst(nw).gt.1)then
	! if(le2+ifirst(1)-1.gt.len(line2))write(6,*)'*',nwords,j,ifirst(1:10),ilast(1:10),line
	! line2(le2+1:le2+ifirst(1)-1)=line(1:ifirst(1)-1)
	! le2=le2+ifirst(1)-1
	! endif
	!		write(6,*)'<22>',j,nw,ifirst(nw)
!	if(icom.lt.le.and.(j.ge.icom0.and.j.lt.icom.or.j.gt.icom.and.line(j:j).eq.' '))then
if(iscom.and.j.eq.icom)then
!	write(17,*)line(1:le)
!	write(17,*)j,icom,le2 Forest
le2n=le2+22+le-j
	line2(le2+1:le2n)='{\color{ForestGreen}'//line(j:le)//'}'
	le2=le2n
	if(ile1.eq.0)le21=le2
	return
endif
	if(isex.and.line(j:j).eq.' ')then
		line2(le2+1:le2+2)='\,'
		le2=le2+2
		goto 100
	endif !if(icom.lt.le.and.(j.ge.icom0.and.j.lt.icom.or.j.gt.icom.a   1706
	if(isex.and.line(j:j).eq.'{')then
		line2(le2+1:le2+2)='\{'
		le2=le2+2
		goto 100
	endif !if(icom.lt.le.and.(j.ge.icom0.and.j.lt.icom.or.j.gt.icom.a   1706
	if(isex.and.line(j:j).eq.'}')then
		line2(le2+1:le2+2)='\}'
		le2=le2+2
		goto 100
	endif !if(icom.lt.le.and.(j.ge.icom0.and.j.lt.icom.or.j.gt.icom.a   1706
	! if(j.eq.icom0)then
		! if(j.gt.1)then
			! if(line(j-1:j-1).eq.'\')then
				! line2(le2:le2)='!'  !replace 
				! icom=le+1
				! icom0=le+1
				! goto 100
			
			! endif
		
		! endif
! !		line2(le2+1:le2+lcomcol+1)=comcol//'!'
! !		le2=le2+lcomcol+1
	
	! endif !if(j.eq.icom0)   1711
	! if(line(j:j).eq.' '.and.(ischar.or.j.gt.icom))then
		! line2(le2+1:le2+2)='\,'
		! le2=le2+2
		! goto 100
	! endif !if(line(j:j).eq.' '.and.(ischar.or.j.gt.icom))   1716
	! if(line(j:j).eq.'_')then
 
		! line2(le2+1:le2+2)='\_'
		! le2=le2+2
		! goto 100
	! endif !if(line(j:j).eq.'_')   1640
	if(p)write(6,*)'<j,nw,ifirst(nw)',j,nw,ifirst(nw)
	if(j.eq.ifirst(nw))then
		lenw=ilast(nw)-ifirst(nw)+1
		ifu=j_isin(line(ifirst(nw):ilast(nw)),j_functions,j_nfunctions_)
		! ifu2=0
		! if(ifu.le.0)ifu2=j_isin(line(ifirst(nw):ilast(nw)+2),j_functions,j_nfunctions_)
	
		iopt=j_isin(line(ifirst(nw):ilast(nw)),j_options,j_noptions_)
		inpufu=j_isin(line(ifirst(nw):ilast(nw)),inpuf,ninpuf)
		! if(index(line(1:le),';sum').gt.0)then
		! write(6,*)'<445ninpuf',ninpuf,inpuf(1:ninpuf)£
		! write(6,*)inpufu
		! stop 'inpu'
		ii1=max(ifirst(nw)-1,1)
		isfu=line(ifirst(nw):ilast(nw)).eq.'enddo'.or.line(ifirst(nw):ilast(nw)).eq.'endif'.or. &
		line(ifirst(nw):ilast(nw)).eq.'then'.and.line(ii1:ii1).eq.')'
		! endif
		!	if(isopt.and.nw.eq.2)write(6,*)'<op',line(ifirst(nw):ilast(nw)),iopt
		if(p)write(6,*)'<44>',nw,ifu,iopt,line(ifirst(nw):ilast(nw)),&
			iopt.gt.0.and.(line(ilast(nw)+1:ilast(nw)+2).eq.'->'.or.(isopt.and.nw.eq.2))
		if(p)write(6,*)'353553,isopt',isopt
		!	iwe=index(line(1:le),'¤')
		! if(index(line(1:le),'perkele').gt.0)then
 
		! write(6,*)'nwwordsetc',nwords,nw,line(1:le),ifirst(1:nwords),ilast(1:nwords)
		! stop
		! endif
		iobj=j_isin(line(ifirst(nw):ilast(nw)),objects,nobjects)
		! if(nw.eq.1.and.firsto)then
			! lis=lobjcol+lenw+1
			! line2(le2+1:le2+lis)=objcol//line(ifirst(nw):ilast(nw))//'}'
			! le2=le2+lis
			! j=j+lenw-1
 
		if(lenw.gt.2.and.line(ifirst(nw):ifirst(nw)).eq.']'.and.line(ilast(nw):ilast(nw)).eq.'[')then
			lis=lobjcol+lenw-1
			line2(le2+1:le2+lis)=objcol//line(ifirst(nw)+1:ilast(nw)-1)//'}'
			le2=le2+lis
			j=j+lenw-1
		elseif(iobj.gt.0)then
			lis=lobjcol+lenw+1
			line2(le2+1:le2+lis)=objcol//line(ifirst(nw):ilast(nw))//'}'
			le2=le2+lis
			j=j+lenw-1
		
			
		elseif(ifu.gt.0.and.line(ilast(nw)+1:ilast(nw)+1).eq.'('.or.isfu)then
			! if(j_functions(ifu)(1:1).eq.';')then
		!	lis=17+lenw
			! lis=linpucolp+lenw
		!	line2(le2+1:le2+lis)='\textcolor{red}{'//line(ifirst(nw):ilast(nw))//'}'
			! line2(le2+1:le2+lis)=inpucol//line(ifirst(nw):ilast(nw))//'}'
 
			! else
		!	idone=0
			!if(ilast(nw).gt.2)then
			! iala=index(line(ifirst(nw):ilast(nw)),'_')
			! if(iala.gt.0)then
			! line2(le2+1:le2+lis)=fucol//line(ifirst(nw):ifirst(nw)+iala-2)//'\'// &
			! line(ifirst(nw)+iala-1:ilast(nw))//'}'
			! lis=lfucolp+lenw+1
			! idone=1
			! endif
			! endif
		!	if(idone.eq.0)then
			lis=lfucolp+lenw
			!	line2(le2+1:le2+lis)='\textcolor{VioletRed}{'//line(ifirst(nw):ilast(nw))//'}'
			line2(le2+1:le2+lis)=fucol//line(ifirst(nw):ilast(nw))//'}'
		!	endif
 
			!			endif
			le2=le2+lis
			j=j+lenw-1
		! elseif(ifu2.gt.0.and.line(ilast(nw)+3:ilast(nw)+3).eq.'(')then
		! !		lis=17+lenw+2
			! ! lis=linpucolp+lenw
		! !	line2(le2+1:le2+lis)='\textcolor{red}{'//line(ifirst(nw):ilast(nw)+2)//'}'
			! ! line2(le2+1:le2+lis)=inpucol//line(ifirst(nw):ilast(nw))//'}'
 
			! ! else
			! lis=lfucolp+lenw+2
			! !	line2(le2+1:le2+lis)='\textcolor{VioletRed}{'//line(ifirst(nw):ilast(nw))//'}'
			! line2(le2+1:le2+lis)=fucol//line(ifirst(nw):ilast(nw))//'\_'//line(ilast(nw)+2:ilast(nw)+2)//'}'
 
			! !			endif
			! le2=le2+lis
			! j=j+lenw-1+2
			! nw=nw+1
			
		elseif(inpufu.gt.0)then
			!	lis=17+lenw
			lis=linpucolp+lenw
			!	line2(le2+1:le2+lis)='\textcolor{red}{'//line(ifirst(nw):ilast(nw))//'}'
			line2(le2+1:le2+lis)=inpucol//line(ifirst(nw):ilast(nw))//'}'
			le2=le2+lis
			j=j+lenw-1
		elseif(line(ifirst(nw):ilast(nw)+1).eq.'sit>')then
			lis=linpucolp+lenw+1
			!	line2(le2+1:le2+lis)='\textcolor{red}{'//line(ifirst(nw):ilast(nw))//'}'
			line2(le2+1:le2+lis)=inpucol//line(ifirst(nw):ilast(nw)+1)//'}'
			le2=le2+lis
			j=j+lenw
 
 
		elseif(iopt.gt.0.and.(line(ilast(nw)+1:ilast(nw)+2).eq.'->'.or.(isopt.and.nw.eq.2)))then
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
			endif !if(isopt)   1785
 
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
		endif !if(nw.eq.1.and.firsto)   1742
		nw=nw+1
		if(nw.gt.nwords)then
		line2(le2+1:le2+le-j+1)=line(j+1:le)
		le2=le2+le-j
		if(p)write(6,*)'hanta/',line2(le2+1:le2+le-j+1),'/',line(j+1:le),'/'
		if(ile1.eq.0)le21=le2
		return
		endif
		!		if(j.ge.ifirst(nw))nw=nw+1
		!		 if(perk)write(6,*)'<39>',nw,j,ifirst(nw)
 
	else
		line2(le2+1:le2+1)=line(j:j)
		if(p)write(6,*)'<23>j,le2',j,le2,line(j:j)
		le2=le2+1
	endif !if(j.eq.ifirst(nw))   1722
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
	le1=j_lentrim(text)
	do i=1,n
		le2=j_lentrim(vector(i))
		if(le1.eq.le2) then
			!20141217 oli: if(text.eq.vector(i)(1:le2))then
			if(text(1:le1).eq.vector(i)(1:le2))then
				j_isin=i
				return
			endif !if(text(1:le1).eq.vector(i)(1:le2))   1864
		endif !if(le1.eq.le2)   1862
	enddo !i=1,n   1860
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
	le=j_lentrim(limit)
	!###TESTAUS###
	!write(6,*)'nextword <526> ial,le,lop,limit ', ial,le,lop,'/',limit(1:le),'/'
	if(ial+le-1>lop) then
		j_nextword = lop+1
		return
	endif !if(ial+le-1>lop)   1893
 
	i = index(inp(ial:lop),limit(1:le))
	!###TESTAUS###
	!write(6,*)'nextword <534> i, inp(ial:lop),limit ',inp(ial:lop),' ',limit
	if(i>0) then
		j_nextword = ial+i-1
	else !if(i>0) then
		j_nextword = lop+1
	endif !if(i>0)   1901
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
	enddo !j=1,le1   1914
	le=jj
	if(le.lt.le1)then
		text(jj+1:le1)=' '
	endif !if(le.lt.le1)   1920
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
		endif !if(ichar(inp(i:i)).gt.32)   1937
	enddo !i=ial,lop   1936
	nonblank=lop+1
	return
end function nonblank !function nonblank(inp,ial,lop)
 
subroutine words(inp,ial0,le,nwords,ifirst,ilast,iss)
	character*(*) inp
	character*25 limit
	integer,parameter::nl=25
	logical islimit
	logical iss
	integer ::ifirst(1:*),ilast(1:*)
	logical p
	!write(6,*)'<1>',ial0,le,inp(ial0:le)
!p=index(inp(ial0:le),'theta'.gt.0
	limit(1:22)=' ,():!&-+/#@".<>=~\%$*'
	limit(23:25)="'{}"
	nwords=0
	islimit=.false.
	ial=ial0
	iss=.false.
	do j=1,le
		if(.not.(inp(j:j).eq.' '.or.ichar(inp(j:j)).eq.9))then
			nonblank=j
			exit
		endif
	
	enddo
	do j=1,le
		if(.not.(inp(j:j).eq.'!'.or.inp(j:j).eq.' '.or.ichar(inp(j:j)).eq.9))then
			if(j.eq.le)then
				iss=.true.
				return
			else
				goto 100
			endif !if(j.eq.le)   1962
		endif !if(.not.(inp(j:j).eq.'!'.or.inp(j:j).eq.' '.or.ichar(inp(j   1961
 
	enddo !j=1,le   1960
 
 
100 continue
main: do j=ial,le
if(inp(j:j).eq.'_')cycle main
sub: do k=1,nl
			if(inp(j:j).eq.limit(k:k).or.ichar(inp(j:j)).eq.9)then
 
				cycle main
			endif !if(inp(j:j).eq.limit(k:k).or.ichar(inp(j:j)).eq.9)   1976
		enddo sub !: do k=1,nl   1975
		nwords=nwords+1
		ifirst(nwords)=j
		!write(6,*)'ifirst ',j,nwords,ifirst(nwords)
		if(ifirst(nwords).ge.le)then
			ilast(nwords)=le
			return
 
		endif !if(ifirst(nwords).ge.le)   1984
		!write(6,*)'goto 2',ifirst(nwords)+1,le
		goto 2
	enddo main !n: do j=ial,le   1974
	!write(6,*)'<2 return'
	if(nwords.eq.0)then
		ifirst(1)=le+1
		ilast(1)=le+1
	endif !if(nwords.eq.0)   1993
	return
 
2	continue
main2: do j=ifirst(nwords)+1,le
sub2: do k=1,nl
			if(inp(j:j).eq.limit(k:k).or.ichar(inp(j:j)).eq.9)then
				ilast(nwords)=j-1
				ial=j
				goto 100
			endif !if(inp(j:j).eq.limit(k:k).or.ichar(inp(j:j)).eq.9)   2002
		enddo sub2 !2: do k=1,nl   2001
 
	enddo main2 !n2: do j=ifirst(nwords)+1,le   2000
 
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
integer function j_lentrim(inp)   !like len-trim but returns zero aslo when line consists of tabs
	character(len=*),intent(in):: inp
	le=len(inp)
 
	if(le.le.0)then
		j_lentrim=0
		return
	endif !if(le.le.0)   2031
	do j_lentrim=le,1,-1
		if(ichar(inp(j_lentrim:j_lentrim)).gt.32)return
	enddo !j_lentrim=le,1,-1   2035
	j_lentrim=0
	return
end function j_lentrim !integer function j_lentrim_(inp)

function nextlim(inp,ial,lop,limit)
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
 
	
	le=len(limit)
	linp=len(inp)
	
	do  i=ial,lop
		
		do  j=1,le
			if(inp(i:i).eq.limit(j:j))then
				nextlim=i
				return
			endif
		enddo ! j=1,le   4410
	enddo ! i=ial,lop   4396
	!1 continue !do 1 i=ial,lop
	nextlim=lop+1

	
	return
end function nextlim !function j_nextlim(inp,ial,lop,limit)
subroutine final(line,lei,line2,le2,doit)
	character*(*)line,line2
	logical doit
	if(.not.doit)then
	line2(1:lei)=line(1:lei)
	le2=lei
	return
	
	endif
			
			isi=nextlim(line(1:lei),1,lei,'%_#$~')
	!		write(17,*)line(1:lei)
			nsi=0
			ialin=1
			ialout=0  !so far
			do while(isi.le.lei)
				isi1=max(isi-1,1)
		!		write(17,*)'isi',isi, ' merkki ',line(isi:isi)
					nle1=isi-ialin
					nle=nle1+1
				!	write(17,*)'nle ',nle
				
				
					
				if(line(isi:isi).eq.'~')then
						line2(ialout+1:ialout+nle1+6)=line(ialin:isi1)//'$\sim$'
						
						ialout=ialout+nle1+6
				!		write(17,*) 'afttilde ',line2(1:ialout)
						nsi=nsi+1
				
				elseif(line(isi1:isi1).ne.'\')then
					nsi=nsi+1
					
			!		write(17,*)'nsi,nle1,nle, ',nsi,nle1,nle,' ialout',ialout,' ialin ',ialin
					if(isi.eq.1)then
						line2(1:1)='\'
					else
						line2(ialout+1:ialout+nle)=line(ialin:isi1)//'\'
				!		write(17,*)'here ',line2(1:ialout+nle)
					endif
					line2(ialout+nle+1:ialout+nle+1)=line(isi:isi)
					ialout=ialout+nle+1

				else
				line2(ialout+1:ialout+nle+1)=line(ialin:isi)
				ialout=ialout+nle
				endif
				ialin=isi+1
			
	!		write(17,*)'outp ',line2(1:ialout)
	!		write(17,*)'inp ',line(1:isi)
				isi=nextlim(line(1:lei),ialin,lei,'%_#$~')
	!		write(17,*)'isi ',isi, 'ialin ',ialin,' lei ',lei
			enddo
			if(ialout.gt.0)then
			line2=line2(1:ialout)//line(ialin:lei)
			le2=ialout+lei-ialin+1
			else
			line2=line(1:lei)
			le2=lei
			endif
	!			write(20,'(a)')line2(1:ialout)//sheaders(j,isec)(ialin:lei)
	 iet=index(line2(1:lei),'&')
	 if(iet.gt.0)line(iet:iet)='!'
! le2=le2+1
! line2(le2:le2)=' '
	 
	 
 end subroutine
 ! \documentclass{report}

! \usepackage{amsfonts, amssymb, amsthm}
! \usepackage{mathtools}
! \usepackage{undertilde}
! \usepackage[utf8]{inputenc}
! \usepackage[english]{babel}
! \usepackage{hyperref}

! \usepackage{thmtools}
! \declaretheoremstyle[
    ! spaceabove=-6pt, 
    ! spacebelow=6pt, 
    ! headfont=\normalfont\bfseries, 
    ! bodyfont = \normalfont,
    ! postheadspace=1em, 
    ! qed=$\blacksquare$, 
    ! headpunct={:}]{mystyle} 
! \declaretheorem[name={Proof}, style=mystyle, unnumbered]{Proof}

! \declaretheoremstyle[
    ! spaceabove=6pt, 
    ! spacebelow=6pt, 
    ! headfont=\normalfont\bfseries,
    ! notefont=\mdseries\bfseries, 
    ! notebraces={(}{)}, 
    ! bodyfont=\normalfont\itshape,
    ! postheadspace=1em,
    ! headpunct={:}]{mystyle}
! \declaretheorem[name={Theorem}, style=mystyle,numberwithin=section]{thm}
! \newtheoremstyle{note}% <name>
! {3pt}% <Space above>
! {3pt}% <Space below>
! {}% <Body font>
! {}% <Indent amount>
! {\itshape}% <Theorem head font>
! {:}% <Punctuation after theorem head>
! {.5em}% <Space after theorem headi>
! {}% <Theorem head spec (can be left empty, meaning `normal')>
!You ca!n change {\itshape} to {\upshape}. On the other hand you can simply use the remark or definition templates.
! Family                 Font Name
! pag                    Avant Garde
! fvs                    Bitstream Vera Sans
! pbk                    Bookman
! bch                    Charter
! ccr                    Computer Concrete
! cmr                    Computer Modern
! pcr                    Courier
! mdugm                  Garamond
! phv                    Helvetica
! fi4                    Inconsolata
! lmr                    Latin Modern
! lmss                   Latin Modern Sans
! lmtt                   Latin Modern Typewriter
! LinuxBiolinumT-OsF     Linux Biolinum (formerly 'fxb' in older package versions)
! LinuxLibertineT-OsF    Linux Libertine (formerly 'fxl' in older package versions)
! pnc                    New Century Schoolbook
! ppl                    Palatino
! qag                    TeX Gyre Adventor 
! qbk                    TeX Gyre Bonum 
! qzc                    TeX Gyre Chorus
! qcr                    TeX Gyre Cursor
! qhv                    TeX Gyre Heros
! qpl                    TeX Gyre Pagella 
! qcs                    TeX Gyre Schola
! qtm                    TeX Gyre Termes
! ptm                    Times
! uncl                   Uncial
! put                    Utopia
! pzc                    Zapf Chancery

! https://gnuplot.sourceforge.net/docs_4.2/node356.html