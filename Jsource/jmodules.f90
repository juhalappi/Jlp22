!the following topics can be seen by finding from files %% concatenated with the topic
!links available for (here there are three %-characters so that topics can be seen by ! searchin %%%
!by searchin with single 5 one can find more details
 
!%%%basic
!%%%char  character functions and variables
!%%%codeoption
!%%%constant
!%%%data handling of data sets
!%%%error
!%%%fig   figures
!%%%function
!%%%input
!%%%io
!%%%jlp
!%%%list
!%%%math   mathematical functions
!%%%matrix
!%%%misc
!%%%simulator
!%%%spline
!%%%stat  statistical functions
!%%%object
!%%%objecttype
!%%%option related to options
!%%%text
!%%title
!%%%trans   transformatio ns
!%%warning
!%%function
module jmod
	double precision,parameter::j_inf=huge(0.d0)
	double precision,parameter::j_ninf=-j_inf
	logical ::j_inited=.false.
	!!module lenmod   !the lengths of character variables
	integer,parameter :: j_lenoption=14
	integer, parameter :: j_lenfunction=16
	integer, parameter :: j_lentitle=15
	integer, parameter :: j_lenobjecttype=16
	integer, parameter :: j_lenform=90
	integer, parameter :: j_lenin=60
	!integer, parameter :: j_lenin_c=120
	!integer, parameter :: j_lendata_subform_c=160
	integer, parameter :: j_lenobject=60
	!end motule !!module lenmod
 
	logical ::j_ispause=.false.
 
 
	!!module j_mod  ! defines e.g. functions, options and object types of basic J
	! use compoundmod
	! 360 deg = 2 pi rad ;; deg = 2 pi/360 rad =  pi/180 rad
	! 360 deg = 2 pi rad ;; rad =360/(2*p) rad = 180/pi rad
	double precision,parameter::j_pi=3.14159265358979323
	double precision,parameter::j_0=0.d0
	double precision,parameter::j_1=1.d0
	integer*8  ::j_18=1
	double precision,parameter::j_m1=-1.d0
	double precision,parameter::j_deg=j_pi/180.d0 !100.d0  !korjaa  j_deg makes degrees to radians
	double precision,parameter::j_todeg=180.d0/j_pi !korjaa !makes radians to degrees
	double precision,parameter::j_e=2.718281828459045
	!options
	character*76 :: j_title  !='j: j3.0 22.04.2022 (c) Juha Lappi and Natural Resources Institute Finland'
	!to be updated in j_init
	!functions
	integer, parameter :: j_nfspec=8
	integer, parameter :: j_nfobj=4
	integer, parameter :: j_nftrans=5
	integer, parameter :: j_nfloop=18
	integer, parameter :: j_nfoper=19
	integer, parameter :: j_nfarit=34
	integer, parameter :: j_nfarit2=5
	integer, parameter :: j_nfdist=5  !distributions
	integer, parameter :: j_nfran=7  !random
	integer, parameter :: j_nfinter=3 !interpola
	integer, parameter :: j_nflist=9  !list function
	integer, parameter :: j_nftex=2  !text
	integer, parameter :: j_nffile=8  !file
	integer, parameter :: j_nfio=7  ! io
	integer, parameter :: j_nfmatr=19  !matrix
	integer, parameter :: j_nfdata=12  !data functions
	integer, parameter :: j_nfstat=13  !stat
	integer, parameter :: j_nfjlp=21 !jlp
	integer, parameter :: j_nfsimu=6 !
	integer, parameter :: j_nffig=6 !
	integer, parameter :: j_nfspli=6 !
	integer, parameter :: j_nfbit=7 !
	integer, parameter :: j_nfmisc=6 !
 
	integer, parameter :: j_fbspec=0   !basis
	integer, parameter :: j_fbobj=j_fbspec+j_nfspec
	integer, parameter :: j_fbtrans=j_fbobj+j_nfobj
	integer, parameter :: j_fbloop=j_fbtrans+j_nftrans
	integer, parameter :: j_fboper=j_fbloop+j_nfloop
	integer, parameter :: j_fbarit=j_fboper+j_nfoper
	integer, parameter :: j_fbarit2=j_fbarit+j_nfarit
	integer, parameter :: j_fbdist=j_fbarit2+j_nfarit2
	integer, parameter :: j_fbran=j_fbdist+j_nfdist
	integer, parameter :: j_fbinter=j_fbran+j_nfran
	integer, parameter :: j_fblist=j_fbinter+j_nfinter
	integer, parameter :: j_fbtex=j_fblist+j_nflist
	integer, parameter :: j_fbfile=j_fbtex+j_nftex
	integer, parameter :: j_fbio=j_fbfile+j_nffile
	integer, parameter :: j_fbmatr=j_fbio+j_nfio
	integer, parameter :: j_fbdata=j_fbmatr+j_nfmatr
	integer, parameter :: j_fbstat=j_fbdata+j_nfdata
	integer, parameter :: j_fbjlp=j_fbstat+j_nfstat
	integer, parameter :: j_fbsimu=j_fbjlp+j_nfjlp
	integer, parameter :: j_fbfig=j_fbsimu+j_nfsimu
	integer, parameter :: j_fbspli=j_fbfig+j_nffig
	integer, parameter :: j_fbbit=j_fbspli+j_nfspli
	integer, parameter :: j_fbmisc=j_fbbit+j_nfbit
	!functions needed in derivatives
	integer,parameter::j_fipower=j_fboper+3
	integer,parameter::j_fmult=j_fboper+4
	integer,parameter::j_fdiv=j_fboper+5
	integer,parameter::j_fplus=j_fboper+6
	integer,parameter::j_fminus=j_fboper+7
	integer,parameter::j_fpower=j_fboper+19
	! !Arithmetic and logical operations after converting to the polish notation
	! 'HMULT','HDIV','IPOWER','MULT','DIV','PLUS','MINUS','EQ','NE','LE','LT','GE', &
	! 'GT','NOT','AND','OR','EQV','NEQV','POWER', & !19
 
	!inout functions
	integer,parameter::j_fputlist=j_fblist+8
 
	!Arithmetic and logical operations after converting to the polish notation
	!	'IPOWER','MULT','DIV','PLUS','MINUS','EQ','NE','LE','LT','GE', &
	!	'GT','NOT','AND','OR','EQV','NEQV','POWER', & !17
	! Arithemetic functions which can operate on scalars or on matrices
 
	! 'min','max','abs','sign','nint','int','ceiling','floor','sqrt','sqrt2', & ! sqrt2(-4)=-2
	! 'log','log10','exp','sin','sind','cos','cosd','tan','tand','cotan', & !d indicates degree argument
	! 'cotand','asin','asind','acos','acosd','atan','atand','acotan','acotand','sinh', &
	! 'cosh','tanh', 'mod','fraction', & !34
	integer,parameter::j_fsqrt=j_fbarit+9
	integer,parameter::j_fsqrt2=j_fbarit+10
	integer,parameter::j_flog=j_fbarit+11
	integer,parameter::j_flog10=j_fbarit+12
	integer,parameter::j_fexp=j_fbarit+13
	integer,parameter::j_fsin=j_fbarit+14
	integer,parameter::j_fsind=j_fbarit+15
	integer,parameter::j_fcos=j_fbarit+16
	integer,parameter::j_fcosd=j_fbarit+17
	integer,parameter::j_ftan=j_fbarit+18
	integer,parameter::j_ftand=j_fbarit+19
	integer,parameter::j_fcotan=j_fbarit+20
	integer,parameter::j_fcotand=j_fbarit+14
	integer,parameter::j_fasin=j_fbarit+21
	integer,parameter::j_fasind=j_fbarit+22
	integer,parameter::j_facos=j_fbarit+23
	integer,parameter::j_facosd=j_fbarit+24
	integer,parameter::j_fatan=j_fbarit+25
	integer,parameter::j_fatand=j_fbarit+26
	integer,parameter::j_facotan=j_fbarit+27
	integer,parameter::j_facotand=j_fbarit+28
	integer,parameter::j_fsinh=j_fbarit+20
	integer,parameter::j_fcosh=j_fbarit+30
	integer,parameter::j_ftanh=j_fbarit+31
 
 
	! ! Special arithemetic functions
	!'der','gamma','loggamma','logistic','npv',
	integer,parameter::j_fgamma=j_fbarit2+2
	integer,parameter::j_floggamma=j_fbarit2+3
	integer,parameter::j_flogistic=j_fbarit2+4
	integer,parameter::j_fassign=j_fbloop+3
	integer,parameter::j_fgoto=j_fbloop+7
	!integer,parameter ::j_fgoto2=j_fbloop+17
	!integer,parameter::j_fgoto3=j_fbloop+18
	integer,parameter::j_fassone=j_fbloop+14
	integer,parameter::j_fpause=j_fbtrans+3
	integer,parameter::j_fsetoption=1
	integer,parameter::j_fsgetelem=2
	integer,parameter::j_fsetelem=3
 
 
	! integer, parameter :: j_nfunctions_ = &
	! 7+   3+     3  +12+  16+      34    +5+    4+   6+    3+   6+   1+  6+    6+  15+ &!!%%function
	! !spec obj,trans,loop arit/log arith arit2 dist, rand,inter,list xt,file,  io ,mat,
	! 8+   13+  19+  6+   5+  6 +  7+    4  ! !number of functions
	! ! dat,stat, jlp simu,fig,splin,bit=7,misc
	integer, parameter :: j_nfunctions_ = j_fbmisc+j_nfmisc
 
	character*(j_lenfunction) :: j_functions(j_nfunctions_) ! function names
 
 
	!	integer dos(j_mxndo)
	!to get the function index in jdotrans the j_nspecialtot (38) must be added to index of the function
	data j_functions/ &
		! Special functions
		'setoption','getelem','setelem','list2', 'o1_funcs','o2_funcs','o3_funcs','setcodeopt', &  ! 8
 
		! Objects
		'type','delete_o','exist_o','name',    &  !4
 
		! Transformations
		'trans','call','pause','noptions','R', &  !5
 
		! Loops and controls structures
		'do', 'if','ASSIGN','bincode','which','errexit', 'goto','itrace', 'trace','tracenow', &
		'itraceoff','traceoff','tracetest','assone','enddo','assmany','goto2','goto3', &  !18
 
		!Arithmetic and logical operations after converting to the polish notation
		'HMULT','HDIV','IPOWER','MULT','DIV','PLUS','MINUS','EQ','NE','LE','LT','GE', &
		'GT','NOT','AND','OR','EQV','NEQV','POWER', & !19
 
		! Arithemetic functions which can operate on scalars or on matrices
 
		'min','max','sign','mod',	'nint','int','ceiling','floor','sqrt','sqrt2', & ! sqrt2(-4)=-2
		'log','log10','exp','sin','sind','cos','cosd','tan','tand','cotan', & !d indicates degree argument
		'cotand','asin','asind','acos','acosd','atan','atand','acotan','acotand','sinh', &
		'cosh','tanh','fraction','abs', & !34
 
		! Special arithemetic functions
		'der','gamma','loggamma','logistic','npv', & !5
 
		! Probability distributions
		'pdf','cdf','bin','negbin','density', &  !5
 
		!Random numbers
		'ran', 'rann', 'ranpoi', 'ranbin', 'rannegbin','select','random', & !6
 
		!! Interpolation
		'interpolate','plane','bilin', & !3
 
		!!! List functions
		'list','merge','difference','index','index_v','len','ilist','putlist','table',& ! 9
 
		!! Creating a text  object
		'text','txt', & !2  text is old txt new
 
		!!File handling
		'exist_f','delete_f','close','showdir','setdir','thisfile', 'filestat','print_f',& ! 8
 
		!! io
		'read','write','print','ask','askc','printresult', 'printresult2',& ! 7 j_fbio
 
		!! Matrices
		'matrix','nrows','ncols','t','inverse','solve', 'qr','eigen','sort','envelope', &
		'find','mean','sum','var','sd','minloc','maxloc','cumsum','corrmatrix',  & !19
 
		!! Data functions
		'data','newdata','exceldata','linkdata','getobs','nobs', 'classvector','values', 'transdata','datawcase',& !11
		'joindata','splitdata',&
		!! Statistical functions
		'stat','cov','corr', 'regr','mse','rmse','coef','r2','se','nonlin', &
		'varcomp', 'classify', 'class', & !13
 
		!! Linear programming
		'problem','jlp','weights','unit','schedcum','schedw','weight','partweights','partunit','partschedcum', &
		'partschedw','partweight','priceunit', 'weightschedcum','priceschedcum','priceschedw', &
		'weightschedw','integerschedw','xkf', 'jlpz','jlpcoef', & ! 21 (Note)
 
		!! Simulator
		'simulatorfree','nextfree','branchfree','simulatefree','cutfree','loadtreesfree', & ! 6
 
		!! Plotting figures
		'plotyx','draw','drawclass', 'drawline','show', 'plot3d',& ! 6
 
		!! Splines, stem splines,  and volume functions
		'tautspline','stemspline','stempolar','laasvol','laaspoly','integrate',  & ! 6
 
		!! Bit function
		'setbits','clearbits','getbit','getbitch','bitmatrix','setvalue','closures', & ! 7
 
		!! Misc. utility functions
		'value','properties','cpu','secnds','where','batch'/ !6
 
	integer,dimension(j_nfunctions_) :: j_minarg_ ! = (/ &  !!!%function
	data j_minarg_/ 1,1,1,1,1,1,1,2,&
		! 'setoption','getelem','setelem','list2', 'o1_funcs','o2_funcs','o3_funcs','setcodeopt', &  ! 8n
 
		0,1,1,1,&
		! 'type','delete_o','exist_o','name',    &  !4
 
		0,1,1,0,1,&
		! 'trans','call','pause','noptions','R', &  !5
 
		2,1,2,0,2,0,1,1,1,0,0,0,0,2,0,2,2,1,&  !loop
		! 'do', 'if','assign','sit','which','errexit', 'goto','itrace', 'trace','tracenow',  &
		! 'itraceoff','traceoff','tracetest','assoneone','enddo','assmany','goto2','goto3',&  !18
 
		2,2,2,2,2,2,1,  2,2,2,   2,2,2,1,2,2,2,2,2,& !foper
		!Arithmetic and logical operations after converting to the polish notation
		!'HMULT','HDIV','IPOWER','MULT','DIV','PLUS','MINUS','EQ','NE','LE', &
		!'LT','GE','GT','NOT','AND','OR','EQV','NEQV','POWER', & !19
 
		1,1,1,31*1, &
		! 'min','max','sign','mod',	'nint','int','ceiling','floor','sqrt','sqrt2', & ! sqrt2(-4)=-2
		! 'log','log10','exp','sin','sind','cos','cosd','tan','tand','cotan', & !d indicates degree argument
		! 'cotand','asin','asind','acos','acosd','atan','atand','acotan','acotand','sinh', &
		! 'cosh','tanh','fraction','abs', & !34
 
		1,1,1,1,2,&
		! 'der','gamma','loggamma','logistic','npv', & !5
 
		1,1,2,2,0,&
		! 'pdf','cdf','bin','negbin', 'density' ,&  !4
 
		0,0,0,1,2,1,1,&
		! 'ran', 'rann', 'ranpoi', 'ranbin', 'rannegbin','select', 'random',& !6
 
		1,4,4,&
		! 'interpolate','plane','bilin', & !3
 
		0,1,2,1,2,1,1,2,2,&
		! 'list','merge','difference','index','index_v','len','ilist','putlist','table',& ! 8
 
		0,0, &
		! 'text','txt', & !2  text is old txt new
 
		1,1,1,0,1,0,1,1,&
		! 'exist_f','delete_f','close','showdir','setdir','thisfile', 'filestat','print_f',& ! 8
 
		3,2,0,0,1,1,1,&
		! 'read','write','print','ask','askc','printresult','printresult2', & ! 7 j_fbio
 
		0,4*1,2,12*1,1,&
		! 'matrix','nrows','ncols','t','inverse','solve', 'qr','eigen','sort','envelope', &
		! 'find','mean','sum','var','sd','minloc','maxloc','cumsum','corrmatrix',  & !19
 
		0,1,1,0,0,0,1,0,0,0,0,1,&
		! 'data','newdata','exceldata','linkdata','getobs','nobs', 'classvector','values', 'transdata','datawcase',& !10
		!linkdata2,splitdata
 
		0,1,1,1,1,1,2,1,1,2,  1,0,1,&
		! 'stat','cov','corr', 'regr','mse','rmse','coef','r2','se','nonlin', &
		! 'varcomp', 'classify', 'class', & !13
 
		0,0,0,4*1,0,11*1,0,1,&
		! 'problem','jlp','weights','unit','schedcum','schedw','weight','partweights','partunit','partschedcum', &
		! 'partschedw','partweight','priceunit', 'weightschedcum','priceschedcum','priceschedw', &
		! 'weightschedw','integerschedw','xkf','jlp2','jlpcoef',  & ! 21  (Note)
 
		0,1,1,0,0,0,&
		! 'simulator','next','branch','simulate','cut','loadtrees', & ! 6
 
		0,0,1,1,1,1,&
		! 'plotyx','draw','drawclass', 'drawline','show','plot3d', & ! 5
 
		1,2,2,2,2,2,&
		! 'tautspline','stemspline','stempolar','laasvol','laaspoly','integrate',  & ! 6
 
		2,1,1,1,0,2,1,&
		! 'setbits','clearbits','getbit','getbitch','bitmatrix','setvalue','closures', & ! 7
 
		6*0/
	! 'value','properties','cpu','seconds','where','batch'/ !6
 
 
	integer, dimension(j_nfunctions_):: j_maxarg_  !=  & !!%%function
	data j_maxarg_/ 4,4,4,99999,9999,9999,9999,3,&
		! 'setoption','getelem','setelem','list2', 'o1_funcs','o2_funcs','o3_funcs','setcodeopt', &  ! 8n
 
		1,1,1,1,&
		! 'type','delete_o','exist_o','name',    &  !3
 
		9999,1,1,0,1,&
		! 'trans','call','pause','noptions','R', &  !4
 
		4,1,2,9999,2,99999,99999,1,1,0,0,0,0,2,0,9999,999,9999,&
		! 'do', 'if','assign','sit','which','errexit', 'goto','itrace', 'trace','tracenow',  &
		! 'itraceoff','traceoff','tracetest','assoneone','enddo','assmany','goto2','goto3',&  !16
 
		2,2,2,2,2,2,1,  2,2,2,   2,2,2,1,2,2,2,2,2,&
		!Arithmetic and logical operations after converting to the polish notation
		!'HMULT','HDIV','IPOWER','MULT','DIV','PLUS','MINUS','EQ','NE','LE', &
		!'LT','GE','GT','NOT','AND','OR','EQV','NEQV','POWER', & !19
 
		9999,9999,2,31*1, &
		! 'min','max','sign','mod',	'nint','int','ceiling','floor','sqrt','sqrt2', & ! sqrt2(-4)=-2
		! 'log','log10','exp','sin','sind','cos','cosd','tan','tand','cotan', & !d indicates degree argument
		! 'cotand','asin','asind','acos','acosd','atan','atand','acotan','acotand','sinh', &
		! 'cosh','tanh','fraction','abs', & !34
 
		1,1,1,1,2,&
		! 'der','gamma','loggamma','logistic','npv', & !5
 
		1,1,2,2,0,&
		! 'pdf','cdf','bin','negbin', 'density' ,&  !4
 
		0,0,0,1,2,1,1,&
		! 'ran', 'rann', 'ranpoi', 'ranbin', 'rannegbin','select', 'random',& !6
 
		1,4,4,&
		! 'interpolate','plane','bilin', & !3
 
		0,9999,2,1,2,1,1,2,2,&
		! 'list','merge','difference','index','index_v','len','ilist','putlist','table',& ! 8
 
		0,0, &
		! 'text','txt', & !2  text is old txt new
 
		1,1,1,0,1,0,1,99,&
		! 'exist_f','delete_f','close','showdir','setdir','thisfile', 'filestat','print_f',& ! 8
 
		3,2,0,0,1,1,1,&
		! 'read','write','print','ask','askc','printresult','printresult2', & ! 6 j_fbio
 
		0,4*1,2,12*1,1,1,&
		! 'matrix','nrows','ncols','t','inverse','solve', 'qr','eigen','sort','envelope', &
		! 'find','mean','sum','var','sd','minloc','maxloc','cumsum','corrmatrix', & !19
 
		0,1,0,0,0,1,0,1,0,0,1,&
		! 'data','newdata','exceldata','linkdata','getobs','nobs', 'classvector','values', 'transdata','datawcase',& !10
		!linkdata2,splitdata
		0,1,1,1,1,1,2,1,1,2,  1,0,1,&
		! 'stat','cov','corr', 'regr','mse','rmse','coef','r2','se','nonlin', &
		! 'varcomp', 'classify', 'class', & !13
 
		0,0,0,4*1,0,11*1,0,1,&
		! 'problem','jlp','weights','unit','schedcum','schedw','weight','partweights','partunit','partschedcum', &
		! 'partschedw','partweight','priceunit', 'weightschedcum','priceschedcum','priceschedw', &
		! 'weightschedw','integerschedw','xkf','jlpz','jlpcoef',  & ! 21  (Note)
 
		0,1,1,0,0,0,&
		! 'simulator','next','branch','simulate','cut','loadtrees', & ! 6
 
		0,0,1,1,1,1,&
		! 'plotyx','draw','drawclass', 'drawline','show','plot3d', & ! 5
 
		1,2,2,2,2,2,&
		! 'tautspline','stemspline','stempolar','laasvol','laaspoly','integrate',  & ! 6
 
		2,1,1,1,0,2,1,&
		! 'setbits','clearbits','getbit','getbitch','bitmatrix','setvalue','closures', & ! 7
 
		6*0/
	! 'value','properties','cpu','seconds','where','batch'/ !6
 
 
 
 
	!20140602 namedobjarg
	integer,parameter::j_ninout=1
	character*(j_lenfunction), dimension(j_ninout)::j_inout
	data j_inout/'putlist('/
 
	integer,parameter ::  j_nnamedfuncarg=1 !63 !!%%function functions which must have
	!                              named objects as arguments
	!use the functions in the same order as they are defined
	character*(j_lenfunction), dimension(j_nnamedfuncarg)::j_namedfuncarg
	data j_namedfuncarg/'delete_o'/
	! data j_namedfuncarg/ &   ! %%function
	! 'stat','call','delete',';incl',';goto','goto','merge','close','read', & !1-10
	! 'ask','askc','index','t','inverse','qr','nonlin','plotyx','simulate','smooth', & !11-20
	! 'regr','nrows','ncols','next','store','load','values','len','sort','show', & ! 21-30
	! 'classify','properties','drawclass','branch','setdir','editdata','rmse','mse','r2','closures', & !31-40
	! 'result','difference','save','unsave','submatrix','der',';trace',';traceoff','trace','traceoff', & !41-50
	! 'tracetest','corr','cov','exist','eigen','getmatrix','xkf','envelope','varcomp','loadmatrix',& ! 51-60
	! 'storematrix',	'utf8','classvector','newdata'/ !61-
 
	! integer,parameter ::  j_nunknownfuncarg=2 !!%%function functions which can have
	! ! ! !                              unknown objects as arguments
 
	! integer, dimension(j_nunknownfuncarg)::j_unknownfuncarg
	! data j_unknownfuncarg/124,181/      !put here function index which can have unknown (but named) objects as arguments
 
 
	!free options free$
	parameter (j_noptions_=213) !!!option number of j_ options
	character*(j_lenoption) :: j_options(j_noptions_) !!!option names of options
	data j_options/'read','in','form','values','data','maketrans','trans', &
		'extra','subextra','mean', 'min','max',& ! 1-10
		'sd','var','warm','volsd','rhs','rhs2','w','obs', 'subobs','problem',& !11-20 MIXED UP
		'from','subdata','nobsw','subread','keep','subkeep','submaketrans','rmse','rows','subin', & !21-30
		'subform','filter','print','sum','duplicate','loglike','mask','maxrounds','test','debug', & !31-40
		'default','classlink','x','maxlines','q','obsw','nobswcum','xrange','yrange','mark',& !41-50
		'keepperiod','nobs','append','variance','oldsubobs','noint','periods','period','unitdata','unitdataobs', & !51-60
		'any','oldobsw','refac','tole','utiltrans','class','key','sparse','row','show',& ! 61-70
		'style','color','func','title','z','out', 'zrange','reject', 'subfilter','subreject',& ! 71-80
		'se','matrix','treevars','plotvars','buffersize','maxtrees','report','modeldf', 'classes','degree',& !81-90
		'minvariance','dummy','sym','minobs','wish','origo','r','colmin','integer','y',&  !91-100
		'points','dx','dy','dz','arg','nonlintrans','corr','corrb','until','angle', &  !101-110
		'model','functions','area','notareavars','subtrans','repeatdomains','options','normal','err','stemcurves',& !111-120
		'relheight','weight','position','exit','iterations','histogram','freq','sort','label','continue','axes','index', & !121-130
		'column','source','diag','width','level','errexit','xlegend','ylegend', & !131-140
		'local','tab','par','wait','file','memory','zmatrix','break', 'content','chi2',&         !141-150
		'expand','exist','slow','initial','eof','object','fastrounds','fast%','finterval','cov', & !151-160
		'sdmean','parmin','parmax','step','dpar','rfhead','rfcode','subrfhead','subrfcode','subnobs',& !161-
		'stop','rfvars','first','last','echo','list','delete','ext','got','do','set','xlabel', & !171-179
		'ylabel','t','coef','sorted','sub','discrete','prolog','epilog','unit',&
		'tailtofirst','tailtolast','cumulative','time','missing','clean','factgroup','dpivot','int','pullout',&
		'basis','condition','fastdif','marksize','keepopen','periodvars','up','maxiter','ilist',&
		'nrowtot'/ !171 - !j_mrfhead=168,j_mrfcode=169,j_mrfsubhead=170
 
	!index for each option corresponds to j_options(j_noptions_) above %%option
	parameter (j_mread=1,j_min=2,j_mform=3,j_mvalues=4,j_mdata=5,j_mmaketrans=6,j_mtrans=7)
	parameter (j_mextra=8,j_msubextra=9)
	parameter (j_mmean=10,j_mmin=11,j_mmax=12,j_msd=13,j_mvar=14,j_mwarm=15)
	parameter (j_mvolsd=16,j_mrhs=17,j_mrhs2=18,j_mw=19,j_mobs=20,j_msubobs=21)   !mobs was mxdata
	parameter (j_mproblem=22,j_mfrom=23)
	parameter (j_msubdata=24,j_mnobsw=25,j_msubread=26,j_mkeep=27,j_msubkeep=28,j_msubmaketrans=29)
	parameter (j_mrmse=30,j_mrows=31,j_msubin=32,j_msubform=33,j_mfilter=34,j_mprint=35)
	parameter (j_msum=36,j_mduplicate=37,j_mloglike=38,j_mmask=39,j_mmaxrounds=40)
	parameter (j_mtest=41,j_mdebug=42,j_mdefault=43)
	parameter (j_mclasslink=44,j_mx=45,j_mmaxlines=46,j_mq=47,j_mobsw=48,j_mnobswcum=49)
	parameter (j_mxrange=50,j_myrange=51,j_mmark=52,j_mkeepperiod=53,j_mnobs=54,j_mappend=55)
	parameter (j_mvariance=56,j_moldsubobs=57,j_mnoint=58,j_mperiods=59,j_mperiod=60,j_munitdata=61)
	parameter (j_munitdataobs=62,j_many=63,j_moldobsw=64,j_mrefac=65,j_mtole=66,j_mutiltrans=67)
	parameter (j_mclass=68,j_mkey=69,j_msparse=70,j_mrow=71,j_mshow=72,j_mstyle=73,j_mcolor=74,j_mfunc=75,j_mtitle=76)
	parameter (j_mz=77,j_mout=78,j_mzrange=79,j_mreject=80,j_msubfilter=81,j_msubreject=82,j_mse=83,j_mmatrix=84)
	parameter (j_mtreevars=85,j_mplotvars=86,j_mbuffersize=87,j_mmaxtrees=88,j_mreport=89)
	parameter (j_mmodeldf=90,j_mclasses=91,j_mdegree=92,j_mminvariance=93,j_mdummy=94,j_msym=95,j_mminobs=96)
	parameter (j_mwish=97,j_morigo=98,j_mr=99,j_mcolmin=100,j_minteger=101,j_my=102,j_mpoints=103)
	parameter (j_mdx=104,j_mdy=105,j_mdz=106,j_marg=107,j_mnonlintrans=108,j_mcorr=109,j_mcorrb=110,j_muntil=111,j_mangle=112)
	parameter (j_mmodel=113,j_mfunctions=114,j_marea=115,j_mnotareavars=116,j_msubtrans=117,j_mrepeatdomains=118)
	parameter (j_moptions=119,j_mnormal=120,j_merr=121,j_mstemcurves=122,j_mrelheight=123,j_mweight=124,j_mposition=125)
	parameter (j_mexit=126,j_miterations=127,j_mhistogram=128,j_mfreq=129,j_msort=130,j_mlabel=131,j_mcontinue=132)
	parameter (j_maxes=133,j_mindex=134,j_mcolumn=135,j_msource=136,j_mdiag=137,j_mwidth=138)
	parameter (j_mlevel=139,j_merrexit=140,j_mxlegend=141,j_mylegend=142,j_mlocal=143,j_mtab=144,j_mpar=145,j_mwait=146,j_mfile=147)
	parameter (j_mmemory=148,j_mzmatrix=149,j_mbreak=150,j_mcontent=151,j_mchi2=152)
	parameter (j_mexpand=153,j_mexist=154,j_mslow=155,j_minitial=156,j_meof=157,j_mobject=158)
	parameter (j_mfastrounds=159,j_mfastp=160,j_mfinterval=161,j_mcov=162,j_msdmean=163)
	parameter (j_mparmin=164,j_mparmax=165,j_mstep=166,j_mdpar=167,j_mrfhead=168,j_mrfcode=169,j_msubrfhead=170,j_msubrfcode=171)
	parameter (j_msubnobs=172,j_mstop=173,j_mrfvars=174,j_mfirst=175,j_mlast=176,j_mecho=177,j_mlist=178,j_mdelete=179,j_mext=180)
	parameter (j_mgot=181,j_mdo=182,j_mset=183,j_mxlabel=184,j_mylabel=185,j_mt=186,&
		j_mcoef=187,j_msorted=188,j_msub=189,j_mdiscrete=190,j_mprolog=191,j_mepilog=192,j_munit=193)
	parameter (j_mtailtofirst=194,j_mtailtolast=195,j_mcumulative=196,j_mtime=197,j_mmissing=198)
	parameter (j_mclean=199,j_mfactgroup=200,j_mdpivot=201,j_mineuueut=202,j_mpullout=203)
	parameter (j_mbasis=204,j_mcondition=205,j_mfastdif=206,j_mmarksize=207,j_mkeepopen=208)
	parameter (j_mperiodvars=209,j_mup=210,j_mmaxiter=211,j_milist=212,j_mnrowtot=213)
	integer,parameter :: j_nnamedoptarg=58
	character*(j_lenoption), dimension(j_nnamedoptarg)::j_namedoptarg
	data j_namedoptarg / & !%%options which should have named objects as arguments
		'read','in','form','data','maketrans','trans', &
		'mean','min','max','sd','var', &
		'rhs','rhs2',&
		'obs','subobs','problem','from','subdata','nobsw','subread','keep','subkeep','submaketrans',&
		'subin', &
		'subform','duplicate','classlink','x','q','obsw','nobswcum','oldsubobs','period','unitdata',&
		'unitdataobs','oldobsw','class','title','z','out','matrix','treevars','plotvars','report','dummy',&
		'y','arg','area','notareavars','subtrans','stemcurves','relheight',&
		'local','file','zmatrix','eof','list','ext' &
		/
 
 
	integer,parameter :: j_nnewvar=37
	character*(j_lenoption), dimension(j_nnewvar)::j_newvar
	data j_newvar / &  !can %%option arguments be generated if they do not exist
		'read','mean','min','max','sd','var','obs','subobs','subdata', &
		'nobsw','subread','sum','x','obsw','nobswcum','oldsubobs','periods', &
		'period','unitdata','unitdataobs','oldobsw','func','out','matrix','treevars','plotvars','report', &
		'dummy','r','y','arg','normal','iterations','local','eof','keep','list'&
		/
 
	!object types
	integer, parameter :: j_nobjecttypes_ = 25 !!!objecttypes
 
	character*(j_lenobjecttype), dimension (1:j_nobjecttypes_)::j_objecttypes !=(/ &
	data j_objecttypes/ & !%%objecttypes
		'REAL','CHAR','free','LIST','MATRIX','MATRIXS','TRANS','TABLE','LISTI','TXT', & !1-10
		'STEMSPLINE','TEXT','DATA','PROB','FIG','SMOOTH','free','REGR','BITMATRIX','free',& !11-20
		'free','free','free','LAASPOLY','TAUTSPLINE'/
 
 
 
	parameter (j_ncodeoptions_=10) ! code!!options
	!	character*(j_lenoption), j_codeoptions(j_ncodeoptions_)  ! code!!options
	!	data j_codeoptions/'filter','reject','subfilter','subreject','variance','weight','func',&
	!	'stop'/
	integer,dimension(j_ncodeoptions_)::j_codeoptions
	data j_codeoptions/j_mfilter,j_mreject,j_msubfilter,j_msubreject,j_mvariance,j_mweight,j_mfunc,&
		j_mstop,j_mloglike,j_mfastp/
 
	!end motule !!module j_mod
 
	!module errmod   ! includes only j_err
	logical :: j_err=.false.
	!end motule !!module errmod
 
	!module j_globalsmod  ! global variables which also own users can access
 
	!character*1,dimension(:),
	character*1, dimension(:),allocatable :: j_ocode
	character*1, dimension(:),allocatable :: j_utf8
	integer :: j_nutf8=0
 
	character(len=j_lenobject) :: j_object_name0 !!!objectname variable for different     !!!object                      purposes
 
	!d=data(read->[,in->][,form->][,nobs->][,maketrans->] [,readfirst->][,trans->]
	![,keep->][,obs->]
	![,filter->][,reject->]
	![,subdata->][,subread->]
	![,subin->][,subform->][,submaketrans->] [,subkeep->][,subobs->]
	![,nobsw->][,nobswcum->][,obsw->] [,duplicate->]
	![,oldsubobs->][,oldobsw->] [,buffersize->][,par->]
 
	integer :: j_leno1_title  !=index(o1_title,' ')
	integer :: 	j_leno2_title !=index(o2_title,' ')
	integer 	j_leno3_title ! =index(o3_title,' ')
 
	!	integer,parameter :: j_maxjlpxvars=800 !!!jlp max number of x-variables in jlpproblem
 
	!global variables related to data function	%%data
	!these can be used when reading data with own format
 
 
 
	character*256 j_buffer   ! used when writing into $Buffer  see user's guide
 
	!	integer, dimension (:), pointer :: j_arg0 !!!function temporary pointer for arguments
	integer, dimension (:), pointer :: j_optarg0,j_optarg !!!option temporary pointerfor !!option arguments
	integer, dimension (:), pointer:: j_arg
	integer, dimension (:), pointer:: j_optarg2,j_optarg3
	!	!
	character*14 j_chrchr  !used in !!chr functions
	character*14 :: j_chrchr0='00000000000000' !used in !!chr functions
 
	! parameter(j_maxcommandbuf=5)  !perhaps not needed
	! integer, dimension(0:j_maxcommandbuf):: j_commandbuf=0
	! integer ::j_nusedcom=0
	! integer,dimension(j_maxcommandbuf) ::j_usedcom=0
	! !
	! integer,dimension(1:j_maxcommandbuf):: j_linecommandbuf=0
	!	integer :: lineobuf = 0
	!logical :: j_erro =.false.
 
	real, parameter :: j_maxwarnings=30.
	!integer ::j_nwarnings=0 j_v(j_ivwarnings) gives the current number of warnings
	! integer,dimension(j_maxwarnings) :: j_wrniob=0
	! integer,dimension(j_maxwarnings) :: j_wrnio=0
	! integer,dimension(j_maxwarnings) :: j_wrnifunc
 
	integer,target:: j_matrix0(0:0)=0  !!!trans empty list for matrix->
 
	integer, target :: j_arglist0(1) = 0 !!!option !!function argument list when there
	!        %%option %%function            there are no arguments
	integer, target :: j_arglist1(1)=-1 !!!option argumentlist when option is not present
	logical :: j_checkedown = .false.  ! are own packages checked
 
 
	integer ::j_nopt =0 !!!option  number of used options for current !module
	integer :: j_nopt2=0  !code options
	integer,parameter ::j_maxopenopt=40
 
	!	integer,dimension(j_maxopenopt2)::j_curropt2
	integer, dimension(j_maxopenopt) :: j_optioniob
	integer, dimension(j_maxopenopt) :: j_optionlink
	integer*2, dimension(2,j_maxopenopt) :: j_optionmoptio
	integer, dimension(j_maxopenopt) :: j_optiontot
	equivalence(j_optiontot,j_optionmoptio)
	integer,parameter::j_maxopenopt2=10
	integer, dimension(j_maxopenopt) :: j_optioniob2
	integer, dimension(j_maxopenopt) :: j_optionlink2
	integer*2, dimension(2,j_maxopenopt) :: j_optionmoptio2
	integer, dimension(j_maxopenopt) :: j_optiontot2
	equivalence(j_optiontot2,j_optionmoptio2)
	!  parameter (j_ifintegerschedcum=144+j_nspecialtot) !??   later
	parameter (j_iretdata=1)     ! possible places for return
	real  :: j_big=1.7e37  !!!misc value used for infinite value
 
	!%%objecttypes
	parameter (j_ipreal=1) !!!objecttype real variable only j_v() can be used
	parameter (j_ipchar=2) !!!objecttype chracter constant
	parameter (j_iplist2=3)  !r
	parameter(j_iplist=4 )    ! type for variable lists
	parameter(j_ipmatrix=5 )     !matrix
	parameter(j_ipmatrixs=6)  !singel precsion matrix
	parameter (j_matreg=1,j_matclass=2,j_matdiag=3,j_matfreq=4)
	!parameter(j_IPRANDOM=6 )     ! random number generator coefficients
	parameter(j_iptrans=7)    ! free named transformations
	parameter(j_iptable=8)    ! stored data in simulations
	parameter(j_ipilist=9 )   !ilist
	!  parameter(j_IPSPLINE=10)  ! spline coefficients
	parameter(j_iptxt=10)
	parameter(j_ipstemspline=11 )   !Callen splini
	parameter(j_iptext=12)  !old text
	parameter (j_ipdata=13)
	parameter (j_ipproblem=14)
	parameter (j_ipfigure=15)
	parameter (j_ipsmooth=16) ! smooth spline
	!parameter (j_ipbits=17) !bitstore
	parameter (j_ipregr=18)
	parameter (j_ipbitmatrix=19)
	!parameter (j_ipstemdata=20)
	!parameter (j_ipstemmodel=21)
	! parameter (j_ipstemcurves=22)
	parameter (j_iptraceset=23)
	parameter (j_iplaaspoly=24)
	parameter (j_iptautspline=25)
	!parameter (ipforestreport=26)
	!parameter (ipstore=17)
	!update  below
	! parameter (ippointer=14) !pointer referring to an other variable
	! parameter(!ipobj =2) ! first objec ????
 
	!used in openread and openwrite to give unitnumber for io
	!!module filemod
	parameter (j_mxunits=23) !max number of units simultaneosuly open
	integer ::j_nunits(j_mxunits)=(/  &
		1,2,3,4,7,8,9,11,12,13,14,15,19,20,21,22,23,24,26,27,28,29,30/)
	integer ::j_readunit(30)=0 ! 27 is the largest of above values
	integer ::j_unitchar(30)=0 ! 0 unit not open,-iv readfile iv write file
 
	!                    27 is the largest of above values
	!                   used only for files associated with characters (iv)
	! units used otherwise in J: 17 and 18 in figures   What else???????
 
	integer :: j_nused =0 !number of used units
	character*250 j_form_  !is used for format in defferent places
	character*160 j_filename  ! chnged from 100 to 160 25 may 2010
	real,dimension(:),allocatable :: j_vector
	double precision,dimension(:),allocatable ::j_dvector
	integer :: j_n_vector=0
	integer :: j_n_dvector=0
	!ten used in jfig, and 16 in jlpdebug
	! 17 and 18 used for fig and flag files
	! 16 used in jlp(
	!!end motule
 
 
	! !module parmod: object indexes for useful global objects, can be obtained also using iv_object('name_of_object')
	! some objects are intial at program start, some later when they are needed
	!objects initilized in
	! integer ivnames  ! ivnames=iv_object('Names')  text objct containing names of all named objects,
	! integer ivpi    ! Pi=v(ivpi)=3.141592653
	! integer ivresult !  output object when no explicit output is given, if output is real variable then
	! !                v(ivresult) gets the value of output, otherwise o(ivresult) is the output object
	! integer ivarg ! Arg  ?miten käytetään
	! integer ivobsdef    ! ivnames=iv_object('Obs') default variable getting observation number in data, note that the nonstandard
	! !  connection between iv... and the variable name
	! integer ivrecord    ! v(ivrecord)  gets the record number when reading data, if records are rejected then
	! !  v(ivrecord) and v(ivobs) are not equal
	! integer ivsubrecord     ! v(ivsubrecord) get the record number of sub-level when reading hierarchical data
 
	! integer ivcursor ! ivnames=iv_object('$Cursor$')  transformation used to drive sit>- prompt, '$Cursor$' appears in error messages
	! !                    otherwise user should not use it
	! integer ivcursor2    ! ivnames=iv_object('Cursor2$') subtransformatio
	! integer ivval       ! ivval=iv_object('$Val$') the transformation used to compute the value of " "-expression
	! integer ivtempdata ! = list containg data-objects  ??
	! integer ivlastdata     ! LastData =list containg last data  (used or defined ?)
	! integer ivcurrentdata      ! ??
	! integer ivdollar       ! $ = indicating default channel (screen) and '*' -format in write
	! integer ivxstar ! x#  used in figure plotting
	! integer ivbuffer     ! ivnames=object_iv('Names')
 
 
	! integer ivselected  !tarkista
	! integer ivprintinput !Printinput controls printing in ;incl default (given in j_init for v(ivprintinput)=2
	! integer ivprintoutput  !Printoutput controls printing in ;incl -files, default Printoutput=2
	integer ::j_ivdeffig=0  !tarkista
 
	integer ::p_ivstartedjlp=0  !??
	integer p_ivpomatrix,p_ivpox,p_ivpoobj,p_ivpounit,p_ivpokey,p_ivpos,p_ivpoibaunit
	!	integer j_ivone   ! v(ivone)=1  here ivone does not refer to a named variable but constant
	!	integer j_ivzero   ! v(ivzero)=0
	!!end motule parmod
 
	! !module vmod
	!real, dimension(:),allocatable::v vhere
	double precision, dimension(:),allocatable::j_v  ! real variable associated with each named object +
	integer, dimension(:), allocatable:: j_otitle
	! each intermediate result + constants
	!integer, dimension(:),pointer::lvo   !link from v to o
	integer j_namedv ! current namber of named objects
	integer j_mxnamedv !maximum number of named objects , setted in j_init, possibly read fron j.par
	integer j_nv  ! mitä on ??
	integer ::j_nconstantv=0  ! current number of constants
	integer j_mxconstantv  ! maximum number of constants
	integer j_mxtemporalv  ! maximum number of intermediate results
	parameter (j_mxtemporalv=200)
	parameter (j_mxtemporalv0=100)  !oridinry temporals rest are for dervative
	parameter (j_mxder0=20)
	integer j_mxder
	double precision,dimension(j_mxtemporalv)::j_temporals
	!	integer ::j_temporalbas
	integer j_mxv 	!total maximum number of values
	integer,parameter ::j_mxrecursion=100
	integer,dimension(j_mxrecursion)::j_curline
	integer ::j_recursion=0
 
	! integer ivdebug  ! ??
 
	!objects initialized in j_init
 
	integer,parameter :: j_ivnames=1 ! Names-object, text object containing names of all named objects
	integer,parameter :: j_ivpi=2 ! Pi Pi=v(ivpi)=3.141592653
	integer,parameter :: j_ivzero=3 ! Result !  output when no explicit output is given
	integer,parameter :: j_ivone=4 ! Arg
	integer,parameter :: j_ivinf=5 ! Obs  default variable getting observation number in data !!!note nonstandard
 
	integer,parameter :: j_ivtolast=6 ! Record  gets the record number when reading data, if records are rejected then
	integer,parameter :: j_ivninf=6
	! !  v(ivrecord) and v(ivobs) are not equal
	integer,parameter :: j_ivmaxnamed=7 ! Subrecord v(ivsubrecord) get the record number
	!                                    of sub-level when reading hierarchical data
	integer,parameter :: j_ivrecursion=8 ! $Cursor$  transformation used to drive sit>- prompt, '$Cursor$' appears in error messages
	! !                              otherwise user should not use it
	integer,parameter :: j_ivcursor=9 ! $Cursor2$
	integer,parameter :: j_ivcursor2=10 ! Val the transformation used to compute the value of " "-expression
	integer,parameter :: j_ivvalc=11 !$Data$  !!!!  list containg data-objects  ??
	integer,parameter :: j_ivround=12 ! LastData list containg last data
	integer,parameter :: j_ivchangep=13 ! Data
	integer,parameter :: j_ivimp=14 ! $ indicating default channel (screen) and '*' -format in write
	integer,parameter :: j_ivlastdata=15 ! x# used in figure plotting
	integer,parameter :: j_ivaccepted=16 ! Selected
	integer,parameter :: j_ivactivep=17 !Printinput rintinput controls printing in ;incl
	!                                default (given in j_init for v(ivprintinput)=2
	integer,parameter :: j_ivrecord=18 !Printoutput Printoutput controls printing in ;incl -files,
	integer,parameter :: j_ivsubrecord=19 ! Duplicate
	integer,parameter :: j_ivduplicate=20 ! $Buffer
	integer,parameter :: j_ivinput0=21 ! $Input0$
	integer,parameter :: j_ivinput1=22 ! $Input1$
	integer,parameter :: j_ivinput2=23 ! $Input2$
	integer,parameter ::j_ivbis=24
 
	integer,parameter :: j_ivb=25 !
	integer,parameter :: j_ivb2=26 !
	integer,parameter :: j_ivdi=27 !
	integer,parameter :: j_ivdi2=28 ! How direct access determines the record length,for  gfortan 4 for
	integer,parameter ::j_ivdg=29
	integer,parameter ::j_ivdg2=30
	integer,parameter ::j_ivbn=31
	integer,parameter ::j_ivbn2=32
 
	! integer,parameter ::j_ivbn2=33
	! integer,parameter ::j_ivbn2=34
	! integer,parameter ::j_ivbn2=35
	! integer,parameter ::j_ivbn2=36
	! integer,parameter ::j_ivbn2=37
	! integer,parameter ::j_ivbn2=38
	! integer,parameter ::j_ivbn2=39
	! integer,parameter ::j_ivbn2=40
	! integer,parameter ::j_ivbn2=41
	! integer,parameter ::j_ivbn2=42
 
 
	integer,parameter ::j_locked=44
 
	!the previsou objects cannot be changed in user trasformationa
	! call j_getobject(0,'Black',j_ipreal,ivout_)
	! j_v(j_ivblack)=j_1
 
	! call j_getobject(0,'Red',j_ipreal,ivout_)
	! j_v(j_ivred)=2.d0
 
	! call j_getobject(0,'Green',j_ipreal,ivout_)
	! j_v(j_ivgreen)=3.d0
 
	! call j_getobject(0,'Blue',j_ipreal,ivout_)
	! j_v(j_ivblue)=4.d0
 
	! call j_getobject(0,'Cyan',j_ipreal,ivout_)
	! j_v(j_ivcyan)=5.d0
 
	! call j_getobject(0,'Violet',j_ipreal,ivout_)
	! j_v(j_ivviolet)=6.d0
 
	! call j_getobject(0,'Yellow',j_ipreal,ivout_)
	! j_v(j_ivyellow)=7.d0
 
	! call j_getobject(0,'Orange',j_ipreal,ivout_)
	! j_v(j_ivorange)=8.d0
	integer,parameter :: j_ivfakematrix=44
	integer,parameter ::j_ivblack=45
	integer,parameter ::j_ivred=46
	integer,parameter ::j_ivgreen=47
	integer,parameter ::j_ivblue=48
	integer,parameter ::j_ivcyan=49
	integer,parameter ::j_ivviolet=50
	integer,parameter ::j_ivyellow=51
	integer,parameter ::j_ivorange=52
 
 
	integer,parameter ::j_ivmaxlines=53
	integer,parameter ::j_ivselected=54
	integer,parameter ::j_ivprintinput=55
	integer,parameter ::j_ivprintoutput=56
	integer,parameter ::j_ivbuffer=57
	integer,parameter ::j_ivdebug=58
	integer,parameter ::j_ivregf=59
	integer,parameter ::j_ivresid=60
	integer,parameter ::j_ivdebugconsole=61
	integer,parameter ::j_ivarg=62
	integer,parameter ::j_ivcontinue=63
	integer,parameter ::j_iverr=64
	integer,parameter ::j_ivresult=65
	integer,parameter ::j_ivdata=66
	integer,parameter ::j_ivdollar=67
	integer,parameter ::j_ivprintresult=68
	integer,parameter ::j_ivdollar2=69
	integer,parameter ::j_ivterminal=70
	integer,parameter::j_ivwindow=71
	integer,parameter::j_ivall=72
	integer,parameter ::j_ivdebugtrans=73
	integer,parameter ::j_ivbgaya=74
	integer,parameter::j_ivcursori=75  !transformation used in input programming
	integer,parameter::j_ivfastp=76
	integer,parameter::j_predefined=76
 
	integer ::p_fastmakes
	type j_basicobject  ! defines basic J-object types
	real, dimension(:),allocatable ::r  ! real vector associated with each  object
	integer, dimension(:),allocatable ::i ! integer vector associated w ith each object
	!	integer*8, dimension(:),allocatable ::i8 ! integer vector associated w ith each object
	integer, dimension(:),allocatable::i2  !second integer vector, contains usually object indexes of subobjects
	double precision, dimension(:),allocatable::d ! double precision vector associatedhh
	character*1,dimension(:), allocatable::ch ! character*1 -vector associated with each object
	character*160,dimension(:),allocatable::txt
	end type j_basicobject !type j_basicobject
 
	character*160,pointer:: j_txtp
 
 
	!j-objects:
	type(j_basicobject),dimension(:),allocatable,target:: j_o
 
 
 
 
 
	type j_datatype  !these can be used when reading data with own functions
	!	integer, dimension(:), pointer :: keep
	integer,dimension(:),pointer::readv
	double precision,dimension(:),allocatable::readvec
	real,dimension(:),allocatable::readvecsing
	end type j_datatype
 
	type(j_datatype),dimension(2)::j_d
 
	integer,dimension(2)::j_nread
	logical,dimension(2)::j_eof
	!!end motule
 
	double precision,dimension(100)::j_tempv
	double precision ::j_dapu,j_dapu2  !for different purposes
	character(len=:), pointer :: j_inprp
	character*3000 j_tempchar
	character*3000 j_tempchar7   !used in j_interpret
	character*13000 j_tempchar2
	character*400 j_tempchar3
	character*60 j_varname1, j_varname2,j_oname,j_oname2,j_oname3
	character*40 j_asktext
	character*160 j_gottext
	integer::j_loname,j_loname2,j_loname3,j_lgottext
	!!end motule
 
	!!module figmod
	integer,parameter::j_gpbas=5   !number of lines before plot line
	logical j_gpshow
	logical j_gpcontinue
	integer j_gpset
	character*60 j_gptitle,j_gpxlabel,j_gpylabel
	character*60 j_gplabel
	integer::j_gplelabel
	integer::j_gppoints
	integer ::j_gpaxes
	integer,parameter::j_gpplots=40   ! must be half
	integer,parameter::j_gplines=500
	integer::j_gpix,j_gpiy
	integer::j_gpletitle,j_gplexlabel,j_gpleylabel
	character*7 ::j_gppt=' pt   '
	character*5:: j_gplw=' lw 2'
	character*5 ::j_gpps=' ps 1'
	!	character*5 ::j_gpps2=' ps 2'
	character*5 ::j_gplt=' lt 1'
 
	character*18,dimension(8)::j_gpcolors
	! set style line 1 lt rgb "red" lw 3
	! set style line 2 lt rgb "orange" lw 2
	! set style line 3 lt rgb "yellow" lw 3
	! set style line 4 lt rgb "green" lw 2
	! set style line 5 lt rgb "cyan" lw 3
	! set style line 6 lt rgb "blue" lw 2
	! set style line 7 lt rgb "violet" lw 3
	! call j_getobject(0,'Black',j_ipreal,ivout_)
 
	! call j_getobject(0,'Red',j_ipreal,ivout_)
	! j_v(j_ivred)=2.d0
	! call j_getobject(0,'Green',j_ipreal,ivout_)
	! j_v(j_ivgreen)=3.d0
	! call j_getobject(0,'Blue',j_ipreal,ivout_)
	! j_v(j_ivblue)=4.d0
	! call j_getobject(0,'Cyan',j_ipreal,ivout_)
	! j_v(j_ivcyan)=5.d0
	! call j_getobject(0,'Violet',j_ipreal,ivout_)
	! j_v(j_ivviolet)=6.d0
	! call j_getobject(0,'Yellow',j_ipreal,ivout_)
	! j_v(j_ivyellow)=7.d0
	! call j_getobject(0,'Orange',j_ipreal,ivout_)
	! j_v(j_ivorange)=8.d0
 
	data j_gpcolors/' lt rgb "black" ',' lt rgb "red" ',' lt rgb "green" ',' lt rgb "blue" ',' lt rgb "cyan" ',&
		' lt rgb "violet" ',' lt rgb "yellow" ',' lt rgb "orange" '/
	logical ::j_gpappend
	integer::j_gpwidth
	integer::j_gpicolor
	integer::j_gpstyle
	integer::j_gpiout
	integer::j_gpnewio
	integer j_gpmark
	integer::j_gpnarg
	integer,dimension(:),pointer::j_gparg
	double precision,dimension(:),allocatable::j_gpval
 
 
	!!end motule
	!!module simumod
	integer j_nper,j_iper,j_ioret,j_keepper,j_ivperiod
	integer, dimension(:),allocatable::j_lastpoint ,j_nextnodes !,igoback
	integer::j_nextnode,j_nextnodeb
	integer,dimension(:,:), allocatable::j_istarts
	integer, dimension(:),allocatable::j_gettreevars,j_loctreevars
	integer,dimension(:),allocatable:: j_dotlist
	! tähän
	integer j_ivtreedata,j_ivntree,j_itree1,j_itree2,j_ivkeeptree,j_ivtreemat
	integer j_ivsimu   !current simulator
	!logical, dimension(:),pointer::gottrt
	!logical gottrt
	!!end motule
	logical j_yes,j_yes2 !logical for different purposes
	!!module showmod
	integer j_ivrfile,j_icolors
	integer j_ivgo_on
	integer*8 j_i8
	!!end motule
	!!module tracemod
	!integer :: j_level
	integer ::j_ivtracevars=0
	integer ::j_ivtracelevel=0
	integer ::j_ivtracecount=0
	integer ::j_ivtracestatus=0
	integer ::j_ivtracemin=0
	integer ::j_ivtracemax=0
	integer ::j_ivtraceminstatus=0
	integer ::j_ivtracemaxstatus=0
	integer ::j_ntrace =40
	integer ::j_ntraced
	logical ::j_deleter
	integer, dimension(:),allocatable::j_traceline,j_traceiv,j_traceii
	integer,parameter::j_txtlen=160
	character*160,dimension(:),allocatable::j_temptxt
	double precision,dimension(:),allocatable::j_tempvector,j_tempvector2
	integer,dimension(:),allocatable::j_itempvector,j_itempvector2,j_itempvector3
	!	integer,dimension(:,:),allocatable::j_itempvector2d
 
	!object-types
	integer, dimension(:),allocatable ::j_otype
 
	!logical,dimension(:),allocatable::j_locked
	integer,dimension(:),allocatable::j_iob,j_io
	! integer, dimension(:),allocatable ::j_otype2
	!integer, dimension(:),allocatable::otype
	! integer no  !number
	!nv=namedv+temporals
	!predefined
	! !end motule vmod
 
	! !module clinemod  character line used for different purposes
	character*80 j_cline
	integer j_lencline ! lentrim(cline)
	character*30 j_pauseprompt
	!!end motule
 
	!!module datamod neede in data-funktion perhaps should be in j_specialmod
	! integer ,dimension(:), pointer :: j_datasets=>null()
	! integer, dimension(:),allocatable::j_curdatasets
	! integer, dimension(:),allocatable::j_iobcur,j_nobcur,j_iobcum
	!	integer j_ndatasets
 
	!	integer j_levels,j_level
	integer::j_divdata,j_dfilterlink,j_drejectlink,j_divtrans,j_divvars,j_dimat,j_divvars2
	integer::j_dnobs
	integer*8::j_dnobs8
	integer::j_dnkeep,j_divkeep,j_dimatup,j_divkeepup,j_dnkeepup,j_divnobsw
	!integer::j_dnkeep,j_divkeep,j_dimatup,j_divkeepup,j_dnkeepup,j_divnobsw
	integer::j_divobsup,j_divobsw,j_divobs,j_diob,j_dobsup
	logical::j_disup,j_distrans,j_disreject,j_disfilter,j_disprint
	integer*8::j_dfrom,j_duntil,j_diba,j_dibaup,j_dnextobs
	integer ::j_dprint,j_depilog
 
	integer::j_dnup
	integer,parameter:: j_dmaxlevels=20
	integer,dimension(1:j_dmaxlevels)::j_dimat2,j_divkeep2,j_dnkeep2,j_divnobsw2,j_divobsw2
	integer,dimension(1:j_dmaxlevels)::j_diba2,j_dobs2,j_dnobswindex,j_dnobs2
	integer,dimension(1:j_dmaxlevels)::j_divobs2
	!integer,dimension(1:j_dmaxlevels)::j_dnobsw2,j_dobsw2
	!integer  ::j_dlevel0
 
	!integer j_level,j_levelm
 
	integer ::j_dlastdata
 
	integer j_filterlink,j_subfilterlink,j_rejectlink,j_subrejectlink
	integer j_iobdata
	integer j_ivtransopt,j_ivtransoptafter
	logical j_filter,j_reject,j_rejected,j_transopt,j_transoptafter,j_subfilter,j_subreject
 
	logical j_remain
	logical j_stop
	logical j_p,j_p2,j_p3
	logical :: j_inpara=.false.
	!	logical j_initialized:: .false.
 
	!!end motule
	!datafmod : dataf-funktion käyttöön
	!!module datafmod
	! lre = linkopt(mread), nrvar = o(iob)%i(lre), iobdataf = iob
	integer j_lre, j_nrvar
	integer j_lre2, j_nrvar2
	integer j_lef_
	integer :: j_ivns
	!integer, dimension(:),pointer::istracemin,istracemax !0 not, 1 yes, 2 errexit
	!real, dimension(:),pointer::tracemin,tracemax
	!module j_specialmod !!module for special purposes, not intended for own functions
	!used for getting chained data in data-function !<modcom> !<modcom>
	!!module linkrmod !<modcom> !<modcom>
	integer :: j_ivswap=0  !data  stored in file
	integer :: j_ivswapm=0  !data matrix stored in file
	type j_linkr
	integer ::iout_trans  !used in trans so that iout must not store in recursion
	double precision, dimension(:),pointer::rbuf=>null()
	type(j_linkr),pointer ::pnext=>null()
	end type !type j_linkr
	!!end motule
 
	!!module jumpmod used to
	parameter (j_mxjump=10)
	integer j_jumpiob(j_mxjump)
	integer j_jumpio(j_mxjump)
	integer j_njump
	! !end motule
 
	!!module loopmod
	!use jmod, only: mxdo
	! parameter (mxdo=8) !same as in jcompil ??
	!parameter (mxiter=100000) later
	parameter  (j_mxdo=8)
	integer j_iido_loop(8,j_mxdo)   !note in inpmod there is also iido
	integer:: j_ndo_loop=0
	!!end motule
	integer j_incline1,j_incline2
 
 
	!!module inpmod used in inputprogramming
	! integer ivinput0,ivinput1,ivinput2
	parameter (j_mxinc=6,j_mxndo=6,j_mxniifs=6,j_mxinc2=20)
	character*14096  j_inp ! obtained input line
	! character*256 inpold
	character*14096 j_inpr
	character*14096 j_inpr2
	character*40 j_adr
	parameter(j_incnu=31)
	integer::j_ninc=1
 
	!	integer :: j_nul(-1:j_mxinc)
	logical :: j_incin=.false.
	integer, dimension(1:j_mxinc)::j_ninc2=0
	integer ::j_inciv(j_mxinc)=0 ! iv for file, last line read
	!	integer ::j_incline(j_mxinc)=0 !when reading from buffer
	integer:: j_increturn(j_mxinc,j_mxinc2)=0 !when returning back after using incl in the same file
	integer,dimension  (1:j_mxinc)::j_ndoinc=0
	integer,dimension  (1:j_mxinc)::j_niifsinc=0
	integer,dimension (1:j_mxinc):: j_ivbuf=0
	integer j_icurl(j_mxinc)
	integer j_ndo
	integer j_ials(0:20)  !satrting points in contianuation lines
	INTEGER        j_iido(7,j_mxndo)
	integer j_dostart(j_mxndo)
	! integer j_printdo(j_mxndo)
	logical :: j_printdo=.false.
	logical,dimension (0:j_mxniifs)::j_bypa=.false.
	logical,dimension (0:j_mxniifs)::j_ifdone=.false.
	integer ,dimension (0:j_mxniifs) :: j_niifsindo=0 ! how many loops are open for each ;if
	integer ::j_niifs=0
	integer j_linp,j_ialb,j_linp7,j_linpold,j_linpr2,j_linpr
 
	!data j_nul/j_mxinc,1,5,1,2,3,4,7/  !only unit 5 has meaning other come from
	parameter ( j_lline =250 )       ! Max. length of a command  record
	logical j_savcom(j_mxinc)     !,savdo1
	!* savcom -> save commands for incl file
	!* reacom  -> reading commands from file
	!* search  -> searching for an address
	!
	logical j_reacom(j_mxinc)
	!       logical ,reado1
	! ldo0 uppermost loop done zero times
	integer j_ldo0
	data j_reacom/j_mxinc*.true./
	logical :: j_bufinp=.false.
	integer j_ivout
	logical j_printed
	integer j_inprint2
	integer :: j_nul0wait=0
	integer j_lenwaitfile  !wait-option in ;incl
	character*20 j_waitfile
	!end module jmod
	! jlp *******************************************************************************
	!	module jlpmod
	logical p_isunit
	real p_time0,p_time00
	!integer ::p_dpivot
	type p_rowxkfkey_type
	integer irowfx !irowfx: indeksi, josta xk mja löytyy coeffx- ja irowfkeep-vektoreista
	integer jcurix !(lavennettu) tehtävärivi, jolla avaintehdas-esiintyy
	end type p_rowxkfkey_type !type j_rowxkfkey_type
 
	type(p_rowxkfkey_type), dimension(:), allocatable :: p_rowxkfkey
	! tehtävärivillä esiintyvät yk-mjiin liittyvät yksikön avaintehtaat, päivitetään yksikön vaihtuessa
	! %ivfout: outputmjan indeksi v-vektorissa
	! %iv2elpos: ptl-mjan xmat-sarake
	! %jcurix: (lavennettu) tehtävärivi, jolla avaintehdas-esiintyy
	type p_rowykfkey_type
	integer ivfout
	integer iv2elpos
	integer jcurix
	end type p_rowykfkey_type !type j_rowykfkey_type
 
	type(p_rowykfkey_type), dimension(:), allocatable :: p_rowykfkey
 
 
 
	real*8 p_apu  !
	real*8 p_valiter
	double precision p_fastdif
	! communication with fletcher:
	! matrix A column a(i,0) not utilized here
	real*8, dimension(:),pointer ::p_a
	integer p_iva
	integer,dimension(:),allocatable::p_abas
	real*8, dimension(:),allocatable::p_acol,p_acolapu
	integer, dimension(:),allocatable::p_icolapu
	!	integer, dimension(:),pointer::p_lavecsp
	!real*8, dimension(:),pointer::p_asp
	!coefficients for object row
	real*8, dimension(:),pointer::p_objr,p_objr00,p_matrix
 
	real*8, dimension(:),pointer ::p_objr0,p_objr2
	integer p_ivobjr0,p_ivobjr2
	! first nrow coewfficients are for residuals, then nz coef for z-variables
	! then nd-coefficients for d-variables
	! in one-x formulation the coffifients in a0 are zero except
	! if there is a x-variable in the objective row its coefficient is one in a0
	! in which case elements of objr are the differences xij(u)-xiJ(u) (p.110)
	!		dimension xmi(mxnm),xma(mxnm)!lower and upper bound for each variable
	real*8, dimension(:),allocatable::p_xmi
	real*8, dimension(:),allocatable::p_xma
	!  now the  lower bound is zero ,and the upper bound is one for proportion weights
	! (for area weights the upper bound is the area of the unit)
	! later there can be general lower and upper bounds
	! values of ther basic variables
 
	real*8, dimension(:),pointer::p_x
	real*8, dimension(:),pointer::p_b
	! current rhs, either rhs or rhs2
	real*8, dimension(:),pointer::p_rhscur
	! working rhs, i.e. xps subtracted, see 6.28 p. 110
	real*8, dimension(:),pointer::p_rhsw
	real*8, dimension(:),pointer::p_vc
	integer p_ivx,p_ivb,p_ivrhscur,p_ivrhsw,p_ivvc
	real*8, dimension(:),pointer::p_rhs,p_rhs2
	real*8,  dimension(:),pointer::p_tole
	integer p_ivrhs,p_ivrhs2,p_ivrhsvars,p_ivrhs2vars,p_ivtole
	integer p_ivrhsplus,p_ivrhs2plus
	real*8:: p_tolep,p_tolecur
	real*8::p_small=-1.d100
	real*8::p_epsj=1.d-4  !1.d-3 chnaged 21.12.2009
	real*8::p_epsn=-1.d-4  !-1.d-3 changed 21.12.2009
	real*8::p_one=1.d0
	real*8:: p_onen=-1.d0
	real*8:: p_oneps=1.00000000001d0
	real*8::p_zero=0.d0
	real*8::p_zeroneg=-0.000000001d0
	real*8 p_objf,p_tmax,p_tmax2,p_val,p_valueopt,p_value
	real*8 p_valuek,p_valueopt2,p_objfv,p_xirowold2
	real*8 p_objfprev
	real*8::p_wminerr=-0.02d0
	real*8::p_wminwrn=-0.00000001d0        !-0.0001d0
	real*8::p_wmaxerr=1.02d0
	real*8::p_wmaxwrn=1.0000000001d0 !!  1.0001d0
	real*8::p_xirowold=0.d0
	real*8:: p_tmaxmin=0.d0  !p_tmaxmin=1.d-7  ! 1.d-5  jl 20160816
	integer p_ivtmax,p_ivcolold,p_ivcolnew,p_ivpivotcase,p_ivlr0,p_ivlz0,p_ivlx0,p_ivlf0
	integer:: p_ivpopar=0
	real*8::  p_vcmax
	real*8::  p_tiny78=1.d-9  ! was 1.d-8 changed 27.8.2018 JL jl 20160602 1.d-8
	real*8::  p_tiny78n=-1.d-9  !was 1.d-8 changed 27.8.2018 JL jl 20160602 1.d-8
	real*8::  p_tiny6=1.d-7 !was 1.d-7 changed 27.8.2018 JL
	real*8::  p_wsu,p_rs
 
	integer p_ivproblem
 
	integer p_ivreport,p_nureport,p_iprint
	integer p_niter
 
	logical:: p_issolution = .false.
	!integer:: j_ivvarsx = 0
	integer p_nsumx
 
	integer p_ivrow,p_nxvartot
	integer p_npvar
	! of schedules in unit
	! (Fletcher is using 'ns' used in JLP)
	integer, dimension(:),pointer::p_ns
	! keyschedule for each unit
	integer, dimension(:),pointer::p_keys
	integer, dimension(:),pointer::p_ibaunit
	double precision, dimension(:),pointer::p_vx   !,p_vxpack
	real, dimension(:),pointer:: p_vxpack
	integer p_ivvx
	!	double precision, dimension(j_maxjlpxvars):: p_vxpack2    !fixed
	integer, dimension(:),pointer::p_ixpack
 
	integer p_ivns,p_ivkeys,p_ivibaunit,p_ivvxpack,p_ivixpack
 
	!	integer, dimension(j_maxjlpxvars)::p_ixpack2
	! ix(irow)= 0, no x in the row
	integer, dimension(:),pointer::p_ix,p_xrowcur
	integer, dimension(:),pointer::p_xrow
 
	logical, dimension(:),pointer::p_ixcur,p_ixcur0
	integer, dimension(:),pointer::p_xvars !x-varaibles in the problem
	integer, dimension(:),pointer::p_xvarsarea ! the list of areavars
	integer, dimension(:),allocatable::p_xvarsareatot ! the list of areavars including all
	!	integer, dimension(:),pointer::p_cvarl
	!	double precision, dimension(:),allocatable::p_cvar
	!	integer p_ivcvar
 
	integer p_ivix,p_ivxrowcur,p_ivxrow,p_ivixcur,p_ivixcur0,p_ivxvars,p_ivxvarsarea
	integer p_ivxvarsareatot   !,p_ivcvarl,p_ivcvar
	integer ::p_nimp,p_nimp2
	!****** definition:
	! nonexpanded problem: rows are as in the original problem definition,
	! expanded problem: the problem where the row is associated only with one domain
	!  for each the domain in a domain set the
	!		integer lunit(0:mxd)	! unit of columns of D,
	! lunit(0)=0 is for termination purposes
	integer, dimension(:),pointer::p_lunit
	integer p_ivlunit
	! schedule numbers for columns of D
	integer, dimension(:),pointer::p_isch
	integer p_ivisch
	!	integer ,dimension(:), allocatable::p_irowdomain
	!	integer ,dimension(:), allocatable::p_irowrow !for each expanded problem
	! irowrow tells the the initial row in the nonexpanded problem now irowrow(ir)=ir+1
	integer ,dimension(:), pointer::p_domainbits
	integer p_ivdomainbits
	integer p_ivdomaintrans
	!	integer p_ivrowdomain
	integer p_ialldomain
	integer p_ndoms  !initvec
	!	integer p_ivvars
	integer,dimension(:),pointer::p_vars
	integer p_ivvars
	integer p_nterm
	integer p_n16
 
	integer p_nfx,p_nfxval,p_nfxfyexp
 
	integer p_ifyfact !can it be dropped
	!	integer p_nrowtot
 
	!	real, dimension(:,:), allocatable ::j_xmat  !x-data matrix
	integer p_ivxdatmat
	!double precision, dimension(:), pointer ::p_xmat  !x-data matrix
	real, dimension(:), pointer ::p_xmat  !x-data matrix
	logical*1,dimension(:),allocatable::p_rejects
	integer p_ivxmat,p_ivrejects
	integer p_nunits,p_lx0,p_ndom,p_ivdomain,p_nrow,p_nrowtot  !p_nrow0= number of contraints
	integer p_row0
	integer p_ndiv
	integer p_lopp
	integer p_nxrow !number of rows with x-variables
	integer p_nxrowcur !
	integer p_ndomv !number of integer variables needed to store all domainbits
	integer p_ndomvars
 
	integer, dimension(:),pointer::p_ixcurow,p_ixcubas
	integer p_ivixcurow,p_ivixcubas
	integer ::p_ido1,p_idostep,p_nxrow0
 
	integer, dimension(:),pointer::p_nixcu
	double precision, dimension(:),allocatable ::p_wdiv
	integer, dimension(:),allocatable ::p_iunitdiv,p_isdiv
 
	integer p_ivnixcu,p_ivwdiv,p_iviunitdiv,p_ivisdiv
	integer p_lavec(1)
 
	integer p_ntemp0
	! uudet
	integer, dimension(:),pointer::p_ls,p_lsi
	! value of the (agrregated) x-variables
	!		real p_xmin(0:la),p_xmax(0:la)	! smallest and largest value of x of a row
	! for current unit
	double precision, dimension(:),allocatable::p_xmin,p_xmax
 
	integer p_ivls,p_ivlsi  !,p_ivxmin,p_ivxmax
	! problem formulation using Fletcher subroutines
	!   Note: Fletchers's own LP is working differently,
	!          here we utilize only matrix routines
 
	! max objr'vars
	! *s.t. (I A)*vars=b
	! there are nrows rows in A
	! first nrow  variables  in vars (corresponding to I) are residual variables
	!    which can be postive or negative
 
	! matrix A is stored as dimension a(la,0:mxn)
	! column 0 is not used here (for Fletcher row 0 is for objective coefficients)
 
	! for JLP the problem formulation is
	! max a0'x+b0'z
	! .st. c<Ax +Bz< C          p. 106 JLP-manual
	! one-x formulation: for each row (including row 0, ie.e. the objective row)
	! there is one x- with coefficient 1, or no x-variable
	! nz= number of z-variables
 
	! putting JLP and Fletchert work togeter:
	! matrix B of JLP is stored as nz first column in A (of Fletcher)
	! D-matrix , i.e. d-vectors of JLP (see p. 110) are stored in A of Fletcher
	! after the z-coefficient part (as columns nz+1,...)
	! these d-vectors are changing all the time
 
 
	! parameters of Fletcher:
	!		parameter (mxlws1=1000) !?
	!		parameter (mxws1=1000)     !?
	!   parameter (mxlws1=6000)
 
	!   parameter (mxws1=20000)
	!		parameter (la=50)   !mx number of constraints
 
	!		parameter (mxz=20)  ! mx number of zvars
	!		parameter (mxd=80)  ! mx number of d-vectors mx number of
	! constaints (la) + 1 or 2 or something
	!		parameter (mxn=mxz+mxd) !mx number of columns in A
	!		parameter (mxnm=mxn+la) ! mx number of columns (icluding the I part)
	!		dimension ws(mxlws1),lws(mxlws1)
	!*ls		dimension ls(mxnm)  ! list of columns of H=(I A), first nrow
	! columns are in the basis, this is updated by Fletcher routines
 
	!		integer lsi(mxnm)	! inverse of ls, lsi(i) tells the position of column
	! i of H in ls
 
	! jlp
	!	double precision xps(0:la) ! sums of x-variables over keyschedules
	! xps(irow) = sum of the x-var present in row irow
	double precision, dimension(:),pointer::p_xps
	!	xsmin(0:la),xsmax(0:la) ! smallest and largest possible
	!                                           sums of x-variables
	integer p_ivxps
	double precision, dimension(:),allocatable::p_xsmin,p_xsmax
 
 
	!	testxps(0:la)  !just for testing
	double precision, dimension(:),allocatable::p_testxps
	real, dimension(:),allocatable::p_test
	double precision, dimension(:),allocatable::p_solx  ! used for integer appr.
	double precision, dimension(:),allocatable::p_sumx ! sums of x-variables in each domain (output)
	double precision, dimension(:),allocatable::p_sumxi ! sums of x-variables in each domain (output) integer
 
	logical p_testl
	!logical p_yes
 
 
	!	xvars(-1:100) !xvar-variables of the problem
	!	cvarl(-1:50) !cvar variables of the problem
	!	zvarsl(-1:100) !zvars of the problem , nyt z-muuttujat ovat nimetönnä
	!	zcoefl(-1:1),zcoef0l(-1:1)	! variables storing table for coefficients
	! of zvariables and table for cofficient in the objective row
	!	rhsl(-1:1),rhs2l(-1:1) ! variables for tables of lower and upper
	! bounds in RHS
	!	ix(0:la)		! indexes for xvars for each row (0 =objective row)
 
	! approximation
	real, dimension(:),pointer::p_shpx  ! shadow prices of x-variables in each domain
 
	!	lr(la)		! list of residuals (columns of I), lr0 first are in basis
	!	lri(la)		! inverse list, lri(i) is the position of residual i in lr
	integer, dimension(:),pointer::p_lr,p_lri
 
	!	ld(mxd)		! list of columns of D (x-var part of the A matrix)
	! first ld0 are in basis
	!	ldi(mxd)	! inverse list
	integer, dimension(:),pointer::p_lx,p_lxi
	!	lz(mxz)		! list z-columns, first lz0 are in the basis
	!	lzi(mxz)	!inverse list
	! p_nunits = how many shared units there are  ?????
	integer, dimension(:),pointer::p_lz,p_lzi
	double precision, dimension(:),pointer:: p_redcost
 
	integer p_ivshpx,p_ivlr,p_ivlri,p_ivlx,p_ivlxi,p_ivlz,p_ivlzi,p_ivredcost,p_ivrefac
	integer p_ivvaluek,p_ivvalueopt
	! unit information for columns of D
	integer p_lunit0		! first unit
	!*next	next(0:mxn) !  is used to travel through columns of D so that
	! the columns correponding to same unit are after each other.
	! if last is the last column in the sequence, then next(last)=0
	! next(0) first column
	integer, dimension(:),allocatable::p_next
	!*iprev		iprev(0:mxd)	!when travelling columns of D in reverse order
	! iprev(0)=last
	integer, dimension(:),allocatable::p_iprev
	!	lower(la)	! if lower bound is active
	logical, dimension(:),allocatable::p_lower
	! lbou: is there lower bound   ubou: is there upper bound
	logical, dimension(:),allocatable::p_lbou
	logical, dimension(:),allocatable::p_ubou
	integer ::p_ivlower,p_ivubou,p_ivlbou
	integer p_pivotstep
	integer p_kierstep,p_kierout
	!integer, dimension(:),pointer::p_lbouJ
	!integer, dimension(:),pointer::p_ubouJ
	integer, dimension(:),pointer::p_lowerJ
	logical p_debug
	logical p_post		! is the entering variable positive
	! we maximize or minimize constraint rows
	! until reaching feasible value
	logical p_maxo ! what is the objective
	logical p_feasible	! if we have found feasible solution
	logical p_feasible1   !first feasible
 
	integer p_ivnext,p_iviprev
	!*************************
	!	integer ,dimension(:), pointer::p_nsetr=>null()   !start
	!integer ,dimension(:), pointer::p_nsetd=>null()
	!	integer ,dimension(:), pointer::p_isetd=>null()
	integer ,dimension(:), pointer::p_nvars  =>null()
	integer ,dimension(:), pointer::p_termvars=>null()
	integer ,dimension(:), pointer::p_xtermvars=>null()
	integer ,dimension(:), pointer::p_coefvars=>null()
	integer ,dimension(:), pointer::p_coefplus=>null()
	integer  ,dimension(:), pointer::p_rowofterm=>null()
	integer  ,dimension(:), pointer::p_nterminrow=>null()  !number of terms in row
	integer, dimension(:), pointer ::p_irowxvars
	double precision  ,dimension(:), pointer::p_coef=>null()
	integer p_ivcoef
	double precision    ,dimension(:), pointer:: p_xcoef
	integer p_nxval
	integer ,dimension(:), allocatable:: p_ibatemp,p_nxrowtemp   !,p_ixprow
	integer ::p_mxlws
	double precision    ,dimension(:), pointer::p_coefz
 
	integer p_ivnvars,p_ivtermvars,p_ivxtermvars,p_ivcoefvars,p_ivcoefplus,p_ivrowofterm,p_ivnterminrow,p_ivirowxvars,p_ivxcoef
	integer p_ivibatemp,p_ivnrowtemp,p_ivcoefz
	logical p_nonlin
	integer p_nonlintrans
 
 
	!	double precision p_objfnonlin,p_objpr evnonlin
 
	integer ,dimension(:), pointer::p_ivdomains
	integer, dimension (:), pointer :: p_domvars
	integer, dimension (:), pointer :: p_rowdomvar
	integer,dimension(:),pointer ::p_rowdomnum
	integer ,dimension(:), pointer::p_domainunits
	integer p_ivdomainunits
	!		integer ,dimension(:), alloca table::p_nunitsrow
	integer p_ivdomvars
	logical p_isdomain
	logical p_ispullout
	integer p_pullout1,p_pullout2
 
	real ::p_as,p_asv
	logical ,dimension(:),allocatable:: p_isx ,p_isxval
	integer ,dimension(:), pointer ::p_zvars
	integer::p_zvars0
	integer ,dimension(:),pointer:: p_nxinrow,p_nxrow2
	integer ,dimension(:),pointer:: p_nzrow,p_nzrow2
	integer ::p_ibasclass
 
	integer p_ivisx,p_ivisxval,p_ivzvars,p_ivnxinrow,p_ivnxrow2,p_ivnzrow,p_ivnzrow2
 
	!factories
	logical p_fpresent
	integer,dimension(:),pointer ::p_factgroup
	integer,dimension(:),pointer ::p_fact
	integer,dimension(:),allocatable ::p_ibasfactgroup
	integer ::p_nfactgroup,p_nfact
	integer ,dimension(:),pointer::p_fvarl2,p_nfact2
	integer ,dimension(:),pointer::p_fvarl=>null()
	!factories, prob-fac vars, final fvars, number of factvars
	logical ,dimension(:),pointer:: p_isfx !is  variable a factory-x variable
	logical ,dimension(:),pointer:: p_isfy !is  variable a factory-y variable
	logical ,dimension(:),allocatable:: p_isfxval !Onko tehtävän muuttujaesiintymä tehdas-x-muuttuja
	logical ,dimension(:),allocatable:: p_isfyval !Onko tehtävän muuttujaesiintymä tehdas-y-muuttuja
 
	double precision, dimension(:), allocatable::p_coeffx	! alfa
 
	integer:: p_ivutiltrans
	integer, dimension(:),allocatable::p_irowfxvars ! i:nnen tehdas x-mjaesiintymän xk:n indeksi xk-listassa (ivxk)
	integer, dimension(:),allocatable::p_irowffact	! i:nnen tehdas x-mjaesiintymän tehtaan indeksi factories-listassa (ivfact)
	integer, dimension(:),allocatable::p_irowfkeep	! i:nnen tehdas x-mjaesiintymän xk:n indeksi datassa (ivxmat)
	integer, dimension(:),allocatable::p_irowfyvars ! i:nnen tehdas y-mjaesiintymän ptl-listan indeksi v-vektorissa
	integer, dimension(:),allocatable::p_irowfyfact	! i:nnen tehdas y-mjaesiintymän ptl-listan indeksi v-vektorissa
	integer, dimension(:), allocatable::p_ibafx !j:nnen (alkup.) tehtävärivin tehdas-x-mjien alkukohta yo vektoreissa
	integer, dimension(:), allocatable::p_ibafy !j:nnen (alkup.) tehtävärivin tehdas-y-mjien alkukohta yo vektoreissa
	!	integer p_ivxpart
	integer,dimension(:),pointer::p_xpart,p_fpart,p_upart
	integer, dimension(:),allocatable::p_xkftable
	logical,dimension(:),allocatable::p_xkftablexkf,p_xkftableuxkf
 
	integer,dimension(:),pointer::p_uxkf
	integer ::p_nuxkf
	integer p_knn
	double precision, dimension(:),allocatable::p_nnutil
	integer, dimension(:),allocatable::p_nnind
	! ibafykeep : i:nnen tehdas y-mjaesiintymän  iv2-listan muuttujien alkukohdat vektoreissa
	! ifyvarskeep : tehdas-y-muuttujien iv2-listan muuttujien xmatriisi-sarakkeet
	integer, dimension(:),allocatable::p_ibafykeep
	integer, dimension(:),allocatable::p_ifyvarskeep
	!ixkkeep[i]= xk-listan i:nnen mjan indeksi x-matriisissa
	integer, dimension(:),allocatable::p_ixkkeep
	! ifyvarsxk : tehdas-y-muuttujien iv2-listan muuttujien paikat xk-listassa
	! ifyvfactfact : tehdas-y-muuttujien iv3-listan tehtaiden paikat factories-listassa
	! ibafyfact : tehdas-y-muuttujien iv3-tehdaslistan tehtaiden alkukohdat ifyfactfact-vektorissa
	integer, dimension(:),allocatable::p_ifyvarsxk
	integer, dimension(:),allocatable::p_ibafyfact
	integer, dimension(:),allocatable::p_ifyfactfact
 
	logical, dimension(:),allocatable::p_ixcurfact ! tosi, jos j:nnellä lavennetulla rivillä on tehdas-x-mjia 20131106 lisäys: tai y-mjia
	integer, dimension(:),allocatable::p_nfxinrow		! tehdas x-mjien lkm tehtävärivillä j (alkup. rivit)
	integer, dimension(:),allocatable::p_nfxrow2 	! ei käytetä?
	integer, dimension(:),allocatable::p_nfyinrow		! tehdas y-mjien lkm tehtävärivillä j (alkup. rivit)
	integer:: p_nfxrow,p_nfyrow
 
	integer, dimension(:),allocatable::p_fxrow	! i:nnen lavennetun tehtävärivin, jolla tehdas-x-mjia, rivinro
	integer, dimension(:),allocatable::p_fyrow ! i:nnen lavennetun tehtävärivin, jolla tehdas-y-mjia, rivinro
 
	!  muuttuja-tehdas-taulukko: xkfact(ixk,j) = xk-listan ixk:nnen mjan j:nnen tehtaan indeksi factories listalla
	!  tehtaiden määrä/muuttuja: nxkfact(ixk) = xk-listan ixk:nnen mjan tehtaiden lkm
	! nxkfact !number of factories for each xk
	integer, dimension(:),allocatable::p_nxkfact
	! tehdas-yk muuttujiin liittyvien muunnosten puutavaralaji-/tehdasmja -output muuttujien indeksit
	! fyfactout(ixk,j) = xk-listan ixk:nnen mjan j:nnen tehtaan muunnoksen ouput iv
	integer, dimension(:,:),allocatable::p_fyfactout
	! keyfact(unit,ixk) = yksikön xk-listan ixk:nnen mjan avaintehtaan indeksi factories listalla
	integer, dimension(:,:),allocatable::p_keyfact
 
	! lf: F-matriisin sarakkeiden indeksit, alussa kannassa olevat sarakkeet
	integer ,dimension(:),allocatable::p_lf,p_lfi
	! kannan sarakkeita vastaavat xk-muuttujat ja tehtaat
	integer, dimension(:), allocatable::p_ixkf
	integer, dimension(:), allocatable::p_ixkffact
	! seuraavat/edelliset kantasarakkeet
	integer, dimension(:,:),allocatable::p_nextf
	integer, dimension(:,:),allocatable::p_iprevf
	integer, dimension(:),allocatable::p_lunxkf
	integer, dimension(:),allocatable::p_lunw
 
	! apuvektorit a-matriisin päivitysarvojen laskentaan (xkf-muuttuja kantaan)
	real*8, dimension(:),allocatable::p_value_af
	real*8, dimension(:),allocatable::p_valueopt_af
	real*8, dimension(:),allocatable::p_valuek_af
 
	logical p_isxk0
	logical p_degeneratef
	! jatketaanko seuraavasta tehdasmjasta tarkastelu
	logical p_nextxkf
 
	! suorituksen nopeutus/tietorakenteet laskennassa tarvittaville indekseille
	! vaihtoehto kantaan-laskenta:
	! tehtävärivillä esiintyvät xk-mjiin liittyvät yksikön  avaintehtaat, päivitetään yksikön vaihtuessa
	! %irowfx: indeksi, josta xk mja löytyy coeffx- ja irowfkeep-vektoreista
	! %jcurix: (lavennettu) tehtävärivi, jolla avaintehdas-esiintyy
 
 
 
	! xkf-kantaan -laskenta:
	! vektori, johon kerätään tehtäväriveiltä ptl-tehdas yhdistelmien esiintymät
	! %isxk : onko suoraan rivillä esiintyvä tehdas-xk-mja (true) vai yk:sta purettu (false)
	! %irow : alkuperäinen tehtävärivi
	! %ind : esiintymän indeksi coeffx-vektorissa (isfx = true) tai muunnoksen outpumjan indeksi v-vektorissa (isfx=false)
	type p_xkrv_type
	logical isxk
	integer irow
	integer ind
	end type p_xkrv_type !type j_xkrv_type
 
	type(p_xkrv_type), dimension(:), allocatable :: p_xkrv
	! muuttuja-tehdas-taulukko: xkfact(ixk,j) = xk-listan ixk:nnen mjan j:nnen tehtaan indeksi factories listalla
	! xkfact(ixk,j)%ifact : xk-listan ixk:nnen mjan j:nnen tehtaan indeksi factories listalla
	! %i1xkrv: ptl-tehdas yhdistelmän 1. esiintymän indeksi xkrv-vektorissa
	! %inxkrv : ptl-tehdas yhdistelmän viimeisen esiintymän indeksi xkrv-vektorissa
	type p_xkfsol_type
	integer iunit
	integer ixk
	integer ifact
	! xkf_sol%next: saman yksikön seuraava (mikä tahansa) tehdassrk
	integer next
	real xkf
	end type p_xkfsol_type !type j_xkfsol_type
 
	type(p_xkfsol_type), dimension(:), allocatable :: p_xkfsol
 
 
 
	type p_xkfact_type
	integer ifact
	integer i1xkrv
	integer inxkrv
	end type p_xkfact_type !type j_xkfact_type
 
	type(p_xkfact_type), dimension(:,:), allocatable :: p_xkfact
 
	! aputaulukko lavennettujen rivien hakemiseen alkuperäisen rivin perusteella
	! irow2curix(0,alkup_rivi) = #lavennetut rivit
	! irow2curix(j,alkup_rivi) = alkup. riviin liittyvä j:s lavennettu rivi
 
 
	!	integer, dimension(:,:),allocatable :: p_irow2curix
 
	! xkf-raportointi-funktiota varten
	integer p_ivxk, p_ivfact
	integer ::p_nxk
	integer ::p_nxkf
	integer,dimension(:),allocatable::p_nxkfrows
	integer p_ivfactgroup
	integer,dimension(:),pointer::p_xk
	integer p_ivmatc, p_ivmatx, p_ivkeepc, p_ivkeepx, p_ivunit,p_ivunitvar
	integer p_keepc
	integer p_keepx
	integer p_ivtrans, p_ivsubtrans !, j_p_ivtransc, j_p_ivtransx
	integer p_lunits0, p_ibaunitbas, p_mxd, p_nrowz
 
	!double precision,dimension(:),allocatable::p_valuedif,p_objdif
	integer ::p_ivvaluedif,p_ivobjdif
	! ratkaisu
	! yksiköstä iunit puutavaralajia ixk tehtaaseen ifact kuljettu määrä xkf
	! ixk, ifact indeksejä listojen ivxk, ivfact alkioihin
	!	integer p_ivxdata
	integer p_ivdatax
	integer p_ivdatac !
	integer p_ivs ! index of schedule variable
 
	integer*8 :: p_nstot
	logical p_tried
	integer p_maxrounds
	integer ::p_itran,p_itrans,p_itemp,p_maxns,p_ivcdata  !,p_ncvararea,p_needc
	integer ::p_ivarea,p_nnotareavars,p_nxvararea,p_nxvar,p_ntemp,p_itemp0,p_idomba,p_idombas
	integer,dimension(:),pointer::p_notareavars
	integer ::p_md
	integer ::p_ivfeasible,p_ivunbounded,p_ivobjective,p_ivoptimal,p_ivobjective0
	integer p_ivobjective2
	integer p_ivoutresult
	integer p_ivpivots
	integer ::p_ilres,p_nresw,p_npivotw,p_nkeyfact
	integer p_nxvarareatot
	logical p_sentered
	logical p_echo
	logical p_p,p_p9,p_p8,p_p2
	real*8 p_val_
	real*8 p_rcur
	logical p_justkey
	logical ::p_factnow=.false.  !are factories checked
	logical p_isstop
	logical p_cycling
	logical p_subfilter_,p_subreject_,p_subfilre
	logical p_zopt
	integer p_listapu(1)
	double precision p_apusum
	logical p_fvari
	logical p_zmatrix, p_zobj, p_rhsopt, p_rhs2opt
	integer p_ivzmatrix
	integer p_ivzobj
	double precision,dimension(:),allocatable:: p_xvars1,p_xvars2 !to delete redundant
	double precision ::p_oldsol
	integer ::p_ienterv=-1
	integer ::p_nsame=0
	double precision :: p_againsol
 
	integer :: p_nagain
	logical :: p_again
	logical p_intapp  !is integer approximation computed
	logical p_yes,p_yes2
	logical p_zeroc  !,dimension(:,:),allocatable :: zeroc  !
	logical::p_warm,p_warmf
	!	logical ::p_w222
 
 
	! xkf_sol%next: saman yksikön seuraava (mikä tahansa) tehdassrk
 
 
	integer p_lf0
	integer p_i0_xkfsol,p_lf0_xkfsol
 
	!	integer j_ivchangep,j_ivround,j_iostop,j_ivstop,j_ivimp
	integer p_stoplink
	integer p_fastplink
	logical p_isfastp
 
	logical p_rejectnow_
	integer p_nkeys,p_nkeyf,p_ntote
	integer p_nkeyfactw
	double precision p_objilres
	logical p_route67
 
	!active set
	logical p_fast  ! is the fast option used
	logical p_fastnow ! is the use of active schedules on for the current round
	logical p_fastmake  ! is the active set made
	integer :: p_fastusedsame !  how many rounds the same set of active schedules has been used
	integer ::p_fastusesame ! how many rounds the same set of active schedules will be used
	logical*1,dimension(:),allocatable :: p_fastreject !tells which schedules are not in the active set
	double precision,dimension(:),allocatable:: p_fastvalues  !the values of schedules in the current unit when setting up the
	double precision p_fastvaluemin ! the minimum value of schedules in the current unit
	double precision ::p_fastpros       ! how big share of the range (fastvaluemin,j_valueopt) (in %) is rejected
	double precision :: p_fastpros2     ! fastpros/100
	double precision ::p_fastcut
	logical*1,dimension(:),allocatable :: p_activeunit
 
 
	logical ,dimension(:),pointer::p_basreject !basic schedules in unit
 
	integer,dimension(1) ::p_loco
	double precision,dimension(:),allocatable:: p_svalue
 
 
	logical p_restarted
	!endfact
 
	logical p_xpresent ! onko dataa
	logical p_xpresent2	! onko oikeasti x-muuttujia
 
	!logical p_needcinx
	character*60 p_domdef
	character*12 p_domname !         $domain12345
	character*350 p_buf
	character*40 p_dots
	character*5 p_apubuf
	data p_dots/'........................................'/
 
	! to be used in Fletcher routines
	integer p_info(1)
 
	!variables used earlier in contains-subroutines
	integer p_ilzmax,p_ienter,p_leave,p_leavec,p_leaved,p_nrowxkfkey,p_nrowykfkey,p_ilrmax
	integer p_iunitv,p_iunit,p_kierv,p_kier,p_newa,p_leavk,p_ia,p_iaft
	integer p_rsame,p_ixkenter,p_ixk_,p_ifopt,p_id
 
	integer p_ixk1_,p_ixk_lkf,p_if_,p_inf_,p_infopt,p_lcursamef,p_lcur0_unit,p_lcur_,p_nz,p_nfy
	integer p_lz0,p_leaz,p_lead,p_iopt,p_lr0,p_lcur,p_lcursame,p_leavkf,p_leavkwf,p_lunit_,p_isecond
	integer p_nrecover,p_pivot,p_idebug,p_ifail
	integer p_refac,p_newc,p_muutosb,p_iunit_lkf,p_ikeepxk_
	integer p_iunitrans,p_nnf,p_nnfiter,p_nnfold2,p_lcur0,p_ix0,p_j1,p_newd,p_nnfold
	!	integer p_irow,p_irow0,p_irow0bas,p_nset
	integer p_noutsubtrans,p_irowinf !p_ncvar,
	integer,dimension(:),pointer::p_icurint,p_icurbit
	integer p_ivicurint,p_ivicurbit
	!	integer p_row0
	integer p_nrejtot,p_nrejdupl
	integer p_mxn,p_mxnm
	integer p_nrefac
	double precision p_coefmax
	integer p_ncol
	integer p_nm
	integer p_nxbas
	integer p_jcurix,p_newf,p_lf01
	integer p_ivoutsubtrans
	!	double precision p_secondb  !the value of the second b est schedule
	logical p_tabu !will leaving schedule be put on fastreject list
	integer p_xdatiba
	logical p_isobjective
	!	logical p_rz,p_doresid
 
	integer p_pivot9,p_iunitprev,p_pivotold
	integer p_iunit55
	integer p_icolold,p_icolnew,p_pivotcase
	integer p_ivpivotcases,p_ivroute67
	integer p_i1
	logical p_goto401
	logical p_goto900,p_goto1234,p_goto1578,p_goto400,p_goto36
	logical p_goto100,p_goto5316,p_goto222,p_goto55,p_goto112233
	logical p_goto8888,p_goto8889,p_goto700 ! input for leaving
	integer p_zerob
	logical p_isobj0,p_isobj2
	! logical p_oto401
 
 
 
 
	!	logical j_xmatinmemory,j_xdatinmemory,j_xdatfromdisk
	!integer j_xmatekaobs,j_xmattokaobs ! buffer observations
	!integer j_xdatekaobs,j_xdattokaobs  !
	!integer j_xmatekabas,j_xmattokabas ! buffer observations
	!integer j_xdatekabas,j_xdattokabas  !
	!	integer j_xmatfirst,j_xdatfirst
	!integer j_xmatlast,j_xdatlast  !last current observation or packed obs in the intitial buffer
	!integer j_xmatlast2,j_xdatlast2 !last observation in the upper buffer
	!integer j_xmatlopp,j_xdatlopp ! last possible obervation in the intial buffer
	!integer j_xmatfirst2,j_xdatfirst2    !j_xmatlopp-1
	!integer j_xmatibas2,j_xdatibas2
	!integer j_xmatnu,j_xdatnu
 
 
 
 
	!end motule !!module j_globalsmod
 
	! !module linkrmod
	! type linkr
	! real, dimension(:),pointer::rbuf=>null()
	! type(linkr),pointer ::pnext=>null()
	! end type
	! !end motule
 
 
 
 
 
 
 
 
 
 
 
	!end motule !!module j_specialmod
 
 
	!!end motule ! j_specialmod
 
	!module getmod
	!<modcom> !<modcom>
	interface j_getoption
		subroutine j_getoption_index(iob,io,moption,minarg,maxarg,iptype,needsarg,noptarg,optarg)
			integer, intent(in):: iob
			integer, intent(in):: io
			integer, intent(in):: moption
			integer, intent(in):: minarg
			integer, intent(in)::maxarg
			integer, intent(in):: iptype
			!logical, intent(in):: expand
			logical, intent(in):: needsarg    !min0 is reserved
			integer,intent(out) :: noptarg
			integer, dimension (:), pointer :: optarg
 
		end subroutine !subroutine j_getoption_index(iob,moption,minarg,maxarg,iptype,expand,needsarg,noptarg,optarg,istart)
 
		subroutine j_getoption_name(iob,io,option,minarg,maxarg,iptype,needsarg,noptarg,optarg)
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
			!integer,optional ::istart
		end subroutine !subroutine j_getoption_name(iob,option,minarg,maxarg,iptype,expand,needsarg,noptarg,optarg,istart)
	end interface !interface j_getoption
 
 
	! interface j_getoption2
	! subroutine j_getoption2_index(iob,io,moption,minarg,maxarg,iptype,needsarg,noptarg,optarg)
	! integer, intent(in):: iob
	! integer, intent(in):: io
	! integer, intent(in):: moption
	! integer, intent(in):: minarg
	! integer, intent(in)::maxarg
	! integer, intent(in):: iptype
	! !	logical, intent(in):: expand
	! logical, intent(in):: needsarg    !min0 is reserved
	! integer,intent(out) :: noptarg
	! integer, dimension (:), allocatable :: optarg
 
	! end subroutine !subroutine j_getoption_index(iob,moption,minarg,maxarg,iptype,expand,needsarg,noptarg,optarg,istart)
 
	! subroutine j_getoption2_name(iob,io,option,minarg,maxarg,iptype,expand,needsarg,noptarg,optarg)
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
	! !integer,optional ::istart
	! end subroutine !subroutine j_getoption_name(iob,option,minarg,maxarg,iptype,expand,needsarg,noptarg,optarg,istart)
	! end interface !interface j_getoption
 
	! interface j_linkcodeoption     NOT NEEDED
	! function j_linkcodeoption_index(moption)
	! integer, intent(in):: moption
	! end function
 
	! function j_linkcodeoption_name(option)
	! character*(*), intent(in):: option
	! end function
	! end interface
	! interface j_putmatrix
	! subroutine j_putmatrix_s(ivmat,irow,icol,val)
	! integer, intent(in):: ivmat
	! integer*4,intent(in) ::irow,icol
	! real, intent(in):: val
	! end subroutine !subroutine j_putmatrix(ivmat,irow,icol,val)
	! subroutine j_putmatrix_d(ivmat,irow,icol,val)
	! integer, intent(in):: ivmat
	! integer*4,intent(in)::irow,icol
	! double precision, intent(in):: val
	! end subroutine !subroutine j_putmatrix(ivmat,irow,icol,val)
	! end interface
 
 
 
	! interface j_chr8
	! character*8 function j_chr8_s(a)
	! real, intent(in):: a
	! end function !character*8 function j_chr8(a)
	! character*4 function j_chr8_d(a)
	! double precision, intent(in):: a
	! end function !character*8 function j_chr8(a)
	! end interface
 
 
 
 
	interface
		! subroutine j_freeunit(nu) ! free unit nu
		! integer,intent(in) ::nu
		! end subroutine !subroutine j_freeunit (nu)
		subroutine j_bypassinc(nu)
			integer,intent(in)::nu
		end subroutine
		subroutine j_parent(inp,le)
			character*(*),intent(inout)::inp
			integer,intent(inout)::le
		end subroutine
 
		subroutine j_jinit(jparf)
			character*(*),intent(inout)::jparf
		end subroutine
 
		subroutine j_clear()
 
		end subroutine
 
		subroutine j_jinit0(jparf)
			character*(*),intent(inout)::jparf
		end subroutine
 
		subroutine j_sub(remain,jparf,error)
			logical,intent(in)::remain
			character*32,intent(in),optional ::jparf
			logical,intent(out)::error
		end subroutine
 
		subroutine j_printlist(nu,iv,head)
			integer,intent(in)::nu,iv
			logical,optional::head
		end subroutine
 
		subroutine j_printoptions()
		end subroutine
 
		subroutine j_printlist0(nu,list0,list,head)
			integer,intent(in)::nu,list0
			integer,intent(in),dimension(list0)::list
			logical,optional::head
		end subroutine
 
		logical function j_writevar(nu,narg,arg,ivform)
			integer,intent(in)::nu,narg,ivform
			integer,intent(in),dimension(narg)::arg
		end function
 
		logical function j_printvar(nu,narg,arg)
			integer,intent(in)::nu,narg
			integer,intent(in),dimension(narg)::arg
		end function
 
		subroutine j_parser(input,ivteku,ivtext)
			character*(*),intent(in)::input
			integer,intent(in)::ivteku
			integer,intent(in),optional::ivtext
		end subroutine
		subroutine j_interpret2(input,ivteku)
			character*(*),intent(in)::input
			integer,intent(in)::ivteku
		end subroutine
 
		subroutine j_readtext(iv)
			integer,intent(in)::iv
 
		end subroutine
 
		subroutine j_func(iob,io,func)
			integer, intent(in):: iob,io
			external ::func
		end subroutine
 
		logical function j_isdollar(iv)
			integer,intent(in)::iv
		end function
 
 
 
		subroutine j_putiounit(nu,iv)
			integer,intent(in)::nu
			integer, intent(in):: iv
		end subroutine
 
		subroutine j_getder(ivfunc,args,ders,nder,old)
			integer,intent(in)::ivfunc
			integer,intent(in)::nder
			integer,intent(in),dimension(nder) :: args
			integer,intent(out),dimension(nder) :: ders
			logical,intent(in),optional::old
		end subroutine
 
 
 
 
		subroutine j_getdos(inp,linp,iargs,nargs)
			integer,intent(in)::linp
			integer,intent(out),dimension(:) :: iargs
			character(len=linp),intent(in)::inp
			integer,intent(out)::nargs
		end subroutine
 
		integer function j_ibass(ivmatrix,iobs,isaa)
			integer,intent(in)::ivmatrix
			integer,intent(in)::iobs,isaa
		end function
 
		integer function j_filesize(ifile,filename,time)
			integer,optional,intent(in)::ifile
			character*(*),optional:: filename
			integer,intent(out),optional::time
		end function
 
		function j_nobsdata(ivdata) !number of obs in data
			integer, intent(in):: ivdata
		end function
 
		! subroutine j_ibass2(ivmatrix,iobs1,iobs2,ibas1,ibas2)
		! integer,intent(in)::ivmatrix,iobs1,iobs2
		! integer,intent(out)::ibas1,ibas2
		! end subroutine
 
 
 
		integer function j_iounit(iv) !gets unit associate with iv
			integer,intent(in) ::iv
		end function !integer function j_iounit(iv)
 
		integer function j_incline() !what was the last incline
 
		end function
 
		subroutine j_inclinegoto(line)
			integer,intent(in)::line
		end subroutine
 
		! integer function j_iounit(iv) !gets unit associate with iv
		! integer,intent(in) ::iv
		! end function !integer function j_iounit(iv)
		subroutine j_getin(iob,io,nu,ivform)
			integer,intent(in)::iob,io
			integer,intent(out)::nu,ivform
		end subroutine
 
		subroutine j_getsubin(iob,io,nu,ivform)
			integer,intent(in)::iob,io
			integer,intent(out)::nu,ivform
 
		end subroutine
 
		subroutine j_getdat(ivdat,nobs,ivmat,ivkeep) !get links to data elements, used getobsiv, used in JLP
			integer,intent(in)::ivdat
			integer,intent(out)::nobs
			integer,intent(out)::ivmat
			integer,intent(out)::ivkeep
		end subroutine !subroutine j_getdat(ivdat,nobs,ivmat,ivkeep,ivtrans,ivvars)
 
		subroutine j_getobsiv(iobs,iomat,ivkeep,iviobs)  !getobs used in JLP, faster than getob
			integer,intent(in):: iobs
			integer,intent(in):: iomat
			integer,intent(in):: ivkeep
			integer,intent(in):: iviobs
		end subroutine !subroutine j_getobsiv(iobs,iomat,ivkeep,iotrans,iviobs)
 
		double precision function j_quad(x,x0,x1,x2,y0,y1,y2) !*qudratic interpolation for point x, Newton forward-form
			double precision, intent(in) :: x,x0,x1,x2,y0,y1,y2
		end function !real function j_quad(x,x0,x1,x2,y0,y1,y2)
 
		integer function j_iprintin(iob,idef) !idef default for print->
			integer,intent(in):: iob,idef
		end function !integer function j_iprintin(iob,idef)
 
		integer function j_iprintout(iob,idef)
			integer, intent(in) :: iob,idef
		end function !integer function j_iprintout(iob,idef)
 
		subroutine j_insert(line,leline0,leline,line2,leline2)
			character*(*),intent(inout) ::line  !object name
			integer,intent(in):: leline0  !lengt of initial part
			integer,intent(inout):: leline  !current and final end of line
			character*(*),intent(in) ::line2  !inserted text
			integer,intent(in)::leline2 ! length of the inserted text
		end subroutine
 
		subroutine j_printtitle(nu,iv)  ! write  !!title of object iv to unit nu
			integer,intent(in) :: nu,iv
		end subroutine !subroutine j_printtitle(nu,iv)
 
		subroutine j_incl(line)
			character*(*)line
		end subroutine
 
		subroutine j_inpugoto(double,inpup)
			logical ::double
			character*(*)::inpup
		end subroutine
 
		integer function j_countlim(inp,linp,lim)
			character*(*)::inp
			integer ::linp
			character*1 ::lim
		end function
 
		subroutine j_pause(prompt,do)
			character*(*),optional::prompt
			logical,optional::do
		end subroutine
 
 
 
		subroutine j_objargs(iob,io) !checking if arguments are named objects
			integer,intent(in):: iob,io
		end subroutine !subroutine j_objargs(iob,io)
 
		subroutine j_copy(iob,io)   !makes a copy of an object
			integer,intent(in)::iob
			integer,intent(in)::io
		end subroutine !subroutine j_copy(iob,io)
		subroutine j_copy2(irg,iout)   !makes a copy of an object
			integer,intent(in)::irg
			integer,intent(in)::iout
		end subroutine !subroutine j_copy(iob,io)
 
 
		! subroutine j_addwarning(iob,io,ifunc)
		! integer, intent(in) ::iob
		! integer, intent(in) ::io
		! integer, intent(in) ::ifunc
		! end subroutine !subroutine j_addwarning(iob,io,ifunc)
 
		subroutine j_errexit()
 
		end subroutine
 
		subroutine j_cleartext(ivtext)	!!!text  clear text object without deleting it
			integer, intent(in):: ivtext
		end subroutine !subroutine j_cleartext(ivtext)
 
		subroutine j_print(iv,ofile,oformat,maxlines,debug)
			integer,intent(in)::iv,maxlines
			character*(*),intent(in)::ofile,oformat
			logical,intent(in)::debug
		end subroutine
 
		subroutine j_checkoutput(iob,io)
			integer, intent(in):: iob,io
		end subroutine
 
		subroutine j_checkoutput0(iob,io)
			integer, intent(in):: iob,io
		end subroutine
 
		subroutine j_startfunction(iob,io,iptype,narg,arg,ivout,delout,iptype2,minarg,maxarg,needsout) !!!function
 
			integer, intent(in):: iob,iptype
			integer,intent(in) :: io
			!	logical, intent(in) :: expand
			integer, intent(out):: narg, ivout
			integer, dimension (:), pointer :: arg
			integer,optional,intent(in):: iptype2,minarg,maxarg
			logical,optional,intent(in)::needsout
			logical,optional,intent(in)::delout
		end subroutine !subroutine j_startfunction(iob,io,iptype,expand,narg,arg,ivout)
 
		subroutine j_command(commandline,passoptions) !execute single !!function from within other function (!!basic)
 
			character*(*), intent(in):: commandline
			logical, optional ::   passoptions   ! are options passed through this J-command
		end subroutine !subroutine j_command(commandline)
 
		! subroutine j_getcommandbuf(ivcommandbuf)
		! integer, intent(out) :: ivcommandbuf
		! end subroutine
 
		! subroutine j_gettitleopt(iob,iout)   ! %%title not needed as startfunc takes care of title
		! integer, intent(in):: iob, iout
		! end subroutine
 
		subroutine j_gettitle(iv,title,lentitle)  !get !!title into char. var title, le is the length
			integer, intent(in):: iv
			integer, intent(out):: lentitle
			character*(*), intent(out)::title
		end subroutine !subroutine j_gettitle(iv,title,lentitle)
 
		integer function j_lentitles(iv)
			integer, intent(in)::iv
		end function j_lentitles !integer function j_lentitles(iv)
 
		integer function j_leno(iv) !the length of object name
			integer, intent(in)::iv
		end function j_leno !integer function j_leno(iv)
 
		!20150812(arg1<->arg2) oli: 		subroutine getv(name,ivin,itype,ivout)
		subroutine j_getobject(ivin,name,itype,ivout,useold,silent)  ! !!object
 
			integer, intent(in):: ivin, itype
			integer, intent(out):: ivout
			character*(*), intent(in):: name
			logical,optional,intent(in)::useold,silent
		end subroutine !subroutine j_getobject(ivin,name,itype,ivout)
 
		subroutine j_getobjectuu(ivin,name,itype,ivout)  ! !!object
 
			integer, intent(in):: ivin, itype
			integer, intent(out):: ivout
			character*(*), intent(in):: name
		end subroutine !subroutine j_getobject(ivin,name,itype,ivout)
 
		subroutine j_getobject2(iv,itype)
			integer, intent(in):: iv, itype
		end subroutine !subroutine j_getobject2(iv,itype)
 
		subroutine j_del(iv) !deletes object, also subobjects  !!object
			integer, intent(in):: iv
		end subroutine !subroutine j_del(iv)
 
		integer*8 function  j_i4i8(i4)
			integer,dimension(2),intent(in)::i4
		end function
 
		subroutine j_i8i4(i8,i4)
			integer,dimension(2),intent(out)::i4
			integer*8,intent(in)::i8
		endsubroutine
 
		subroutine j_del0(iv) !deletes object, NOT subobjects  !!object
			integer, intent(in):: iv
		end subroutine !subroutine j_del0(iv)
 
		logical function j_expandable(iv) ! !!matrix   is matrix expandable
			integer,intent(in) ::iv
		end function !logical function j_expandable(iv)
		! subroutine j_expand(iv,newrows,newcolumns)
		! integer,intent(in) ::iv,newrows,newcolumns
		! end subroutine
 
		integer function j_lentrim(inp)   !like len-trim but returns zero aslo when line consists of tabs
			character(len=*),intent(in):: inp
		end function !integer function j_lentrim_(inp)
 
		subroutine j_delu(iv)
			integer, intent(in):: iv
		end subroutine !subroutine j_delu(iv)
 
		!20150812(arg1<->arg2) oli: 		subroutine deftext(name,iv,lines,leng,ivout)
		subroutine j_deftext(iv,name,lines,leng,ivout) ! defines !!text !!object
			integer, intent(in):: iv, lines,leng
			integer, intent(out):: ivout
			character*(*), intent(in):: name
		end subroutine !subroutine j_deftext(iv,name,lines,leng,ivout)
 
		!20150812(arg1<->arg2) oli: 		subroutine deftext2(name,iv,lines,leng,lines2,ivout)
		subroutine j_deftext2(iv,name,lines,leng,lines2,ivout)
			integer, intent(in):: iv, lines,leng,lines2
			integer, intent(out):: ivout
			character*(*), intent(in):: name
		end subroutine !subroutine j_deftext2(iv,name,lines,leng,lines2,ivout)
 
		!20150812(arg1<->arg2) oli: 		subroutine defmatrix(name,iv,ndim1,ndim2,itype,ivout)
 
		!20150812(arg1<->arg2) oli: 		subroutine defdata(name,iv,ivmat,ivkeep,ivcases,ivprolog,ivmaketrans,ivtrans,&
 
		integer	function j_defmatrix(iv,name,ndim1,ndim2,itype,single,nod,rowtot)
			integer, intent(in):: iv,itype
			integer,intent(in) ::ndim1,ndim2
			character*(*), intent(in):: name
			logical,intent(in),optional:: single
			logical,intent(in),optional:: nod
			integer,intent(in),optional::rowtot
		end function !subroutine j_defmatrix(iv,na
 
 
 
		integer	function j_defmatrix8(iv,name,ndim1,ndim2,itype,single,nod,rowtot)
			integer, intent(in):: iv,itype
			integer*8,intent(in) ::ndim1,ndim2
			character*(*), intent(in):: name
			logical,intent(in),optional:: single
			logical,intent(in),optional:: nod
			integer*8,intent(in),optional ::rowtot
		end function !subroutine j_defmatrix(iv,name,ndim1,ndim2,itype,expand,ivout)
 
		subroutine j_putmatrix(ivmat,irow,icol,val)
			integer, intent(in):: ivmat
			integer*4,intent(in) ::irow,icol
			double precision, intent(in):: val
		end subroutine !subroutine j_putmatrix(ivmat,irow,icol,val)
 
		subroutine j_putmatrix8(ivmat,irow,icol,val)
			integer, intent(in):: ivmat
			integer*8,intent(in) ::irow,icol
			double precision, intent(in):: val
		end subroutine !subroutine j_putmatrix(ivmat,irow,icol,val)
 
 
 
 
 
		subroutine j_defdata(iv,ivmat,ivkeep,ivcases)
			integer, intent(in):: iv,ivmat,ivkeep
			!	integer, intent(in):: ivsub,ivnobsw,ivup,ivobsw,ivnobswcum
			integer, intent(in),optional:: ivcases
 
		end subroutine !subroutine j_defdata(iv,ivmat,ivkeep,ivcases,ivprolog,ivmaketrans,ivtrans,&
		integer function j_datamatrix(iv)  ! !!data
			integer,intent(in) ::iv
			!j_datamatrix=	j_o(iv)%i(1)
		end function !integer function j_datamatrix(iv)
 
		integer function j_datakeep(iv)  ! !!data
			integer,intent(in) ::iv
			!j_datakeep=	j_o(iv)%i(2)
		end function !integer function j_datakeep(iv)
 
 
 
 
 
		integer function j_datasub(iv)  ! !!data
			integer,intent(in) ::iv
			!j_datasub=	j_o(iv)%i(9)
		end function !integer function j_datasub(iv)
 
		integer function j_datanobsw(iv)  ! !!data
			integer,intent(in) ::iv
			!j_datanobsw=	j_o(iv)%i(10)
		end function !integer function j_datanobsw(iv)
 
		integer function j_dataup(iv)  ! !!data
			integer,intent(in) ::iv
			!j_dataup=j_o(iv)%i(11)
		end function !integer function j_dataup(iv)
 
		integer function j_dataobs(iv)  ! !!data
			integer,intent(in) ::iv
			!j_dataobs=j_o(iv)%i(12)
		end function !integer function j_dataobs(iv)
 
		integer*8 function j_nrows(iv)
			integer,intent(in)::iv
		end function
		integer*8 function j_nrowtot(iv)
			integer,intent(in)::iv
		end function
		integer*8 function j_ncols(iv)
			integer,intent(in)::iv
		end function
 
		integer*8 function j_nelem(iv)
			integer,intent(in)::iv
		end function
 
 
		integer function j_dataobsw(iv)  ! !!data
			integer,intent(in) ::iv
			!j_dataobsw=j_o(iv)%i(13)
		end function !integer function j_dataobsw(iv)
 
		integer function j_datanobswcum(iv)  ! !!data
			integer,intent(in) ::iv
			!j_datanobswcum=j_o(iv)%i(14)
		end function !integer function j_datanobswcum(iv)
 
 
 
		!20150812(arg1<->arg2) oli: 		subroutine deftrans(name,iv,leng,ivout,lenin,lenout,ivinl,ivoutl,linsource)
		subroutine j_deftrans(iv,name,ivout,leng,lenin,lenout,ivinl,ivoutl,ivlocal,linsource,ivarg,istrans)
 
			integer, intent(in):: iv,leng,lenin,lenout,linsource
			integer, intent(out):: ivout,ivinl,ivoutl,ivlocal
			integer,optional, intent(in):: ivarg
			character*(*), intent(in):: name
			logical,optional, intent(in):: istrans
		end subroutine !subroutine j_deftrans(iv,name,leng,ivout,lenin,lenout,ivinl,ivoutl,linsource)
		!ivinl(ivtrans) : get the input varaible list for a transforamtion
		integer	function j_trans_input(iv)
			integer, intent(in):: iv
		end function !integer	function j_trans_input(iv)
 
		integer function j_trans_output(ivtrans)
			integer, intent(in):: ivtrans
		end function !integer function j_trans_output(ivtrans)
 
		integer function j_trans_arg(iv)   ! !!trans
			integer, intent(in):: iv
			!j_ivoutl=j_o(iv)%i2(9)
		end function j_trans_arg !integer function j_trans_arg(iv)
 
		integer function j_trans_result(iv)   ! !!trans
			integer, intent(in):: iv
			!j_transresult=j_o(iv)%i2(10)
		end function j_trans_result !integer function j_trans_result(iv)
 
		integer function j_trans_source(iv)   ! !!trans
			integer, intent(in):: iv
			!j_trans_source=j_o(iv)%i2(11)
		end function j_trans_source !integer function j_trans_source(iv)
 
 
		subroutine j_cleartrans(ivtrans)
			integer, intent(in):: ivtrans
		end subroutine !subroutine j_cleartrans(ivtrans)
 
		!gettrans(ivtrans,ivinput,ivoutput)  : get a trans-object
		! subroutine j_gettrans(ivtrans,ivinput,ivoutput)
		! integer, intent(in):: ivtrans
		! integer, intent(out):: ivinput, ivoutput
		! end subroutine
 
		subroutine j_debugerr(iob,io)
 
			integer, intent(in):: iob,io
		end subroutine !subroutine j_debugerr(iob,io)
		subroutine j_debug(iob)
 
			integer, intent(in):: iob
		end subroutine !subroutine j_debugerr(iob,io)
 
		subroutine j_where(iob,io)
 
			integer, intent(in):: iob,io
		end subroutine !subroutine j_debugerr(iob,io)
 
		!puttext(iv,text) : put text into text object
		subroutine j_puttext(iv,text)
 
			integer, intent(in):: iv
			character*(*), intent(in):: text
 
		end subroutine !subroutine j_puttext(iv,text)
 
		!puttext2(iv,text) : store also the line number in h2
		subroutine j_puttext2(iv,text)
			integer, intent(in):: iv
			character*(*), intent(in):: text
		end subroutine !subroutine j_puttext2(iv,text)
 
		!nlines(iv) : number of lines in a tex buffer
		function j_nlines(iv)
			integer, intent(in):: iv
		end function !function j_nlines(iv)
 
		!	putcleantext(iv,text) : as puttext but remove blanks etc
		subroutine j_putcleantext(iv,text)
			integer, intent(in):: iv
			character*(*), intent(inout):: text !NOTE inout
		end subroutine !subroutine j_putcleantext(iv,text)
 
		!putnewcleantext(iv,text,iline) : clean the text and put in the text object if not there
		subroutine j_putnewcleantext(iv,text,iline)
			integer, intent(in):: iv
			integer, intent(out):: iline
			character*(*), intent(in):: text
		end subroutine !subroutine j_putnewcleantext(iv,text,iline)
 
		!printtext(iob,line) : print line of text object, if line=0 -> print all
		subroutine j_printtext(iob,line)
			integer, intent(in):: iob, line
		end subroutine !subroutine j_printtext(iob,line)
 
		!printtext(iob,line) : print line of text object, if line=0 -> print all
		subroutine j_gettext(iob,line)
			integer, intent(in):: iob, line
		end subroutine !subroutine j_printtext(iob,line)
 
 
		!writetext(nu,iob,line) : print line of text object, if line=0 -> print all
		subroutine j_writetext(nu,iob,line,nonum)
			integer, intent(in):: nu,iob, line
			logical,intent(in),optional ::nonum
		end subroutine !subroutine j_writetext(nu,iob,line)
 
		! getline(iv,line,buffer,le) : get line line from text object iv into buffer
		subroutine j_getline(iv,line,buffer,le)
			integer, intent(in):: iv, line
			integer, intent(out):: le
			character*(*), intent(out) :: buffer
		end subroutine !subroutine j_getline(iv,line,buffer,le)
 
		subroutine j_getname(iv,iv2,iv3)
			integer, intent(in):: iv
			integer,intent(in),optional::iv2,iv3
		end subroutine !subroutine j_getline(iv,line,buffer,le)
 
		subroutine j_getname0(iv,name,lname)
			integer, intent(in):: iv
			integer,intent(out) ::lname
			character*(*),intent(out) ::name
		end subroutine !subroutine j_getline(iv,line,buffer,le)
 
		!getline2(iv,line,buffer,le) : as getline but fill end of buffer with blanks
		subroutine j_getline2(iv,line,buffer,le)
			integer, intent(in):: iv, line
			integer, intent(out):: le
			character*(*), intent(out) :: buffer
		end subroutine !subroutine j_getline2(iv,line,buffer,le)
 
		!getchar(iv,buffer,le) : get char constant or char varaible or real var as char*8
		subroutine j_getchar(iv,buffer,le)
			integer, intent(in):: iv
			integer, intent(out):: le
			character*(*), intent(out) :: buffer
		end subroutine !subroutine j_getchar(iv,buffer,le)
 
		!mtja(name) ! get the index of variable with name, if not defined return 0
		function j_object(name)
			character*(*), intent(in) :: name
		end function !function j_object(name)
 
		! !mtjach1(name,lename) : get the index of variable with name, if not defined return 0
		! function j_mtjach1(name,lename)
		! character*(*), intent(in) :: name
		! end function
 
		!mtja2(name,iv) : get the index of variable with name name//name_of_iv if not defined return 0
		function j_object2(name,iv)
			integer, intent(in):: iv
			character*(*), intent(in) :: name
		end function !function j_object2(name,iv)
 
		function j_object3(iv,name,char) !reverse order
			integer, intent(in):: iv
			character*(*), intent(in) :: name
			logical,intent(in),optional::char
		end function !function j_object2(name,iv)
 
		subroutine j_getobjectnam(name,itype,ivout) ! %%object get old or new object, UUSI
			!                  %%object is  ready to use only for real variables or character constants
			! if the object exist and it is not of type real, it will be deleted
			! if name=' ' then get as object ivin
			! if name & ivin=0 then get old or new with the name
			! if name & ivin>0 then get variable which has prefix name of ivin and then come the name
			!if ivin=0 and name starts wit ' the character contant is created even ityp is j_ipreal
			character*(*), intent(in):: name
			integer,intent(in)::itype
			integer, intent(out):: ivout
 
		end subroutine
 
		!line(iv,name) : get line number of text consisting of name, not found =>0
		function j_line(iv,name)
			integer, intent(in):: iv
			character*(*), intent(in) :: name
		end function !function j_line(iv,name)
 
		!line2(ivtext,ivchar) : as line but now the input text is a charvar or charconst
		function j_line2(ivtext,ivchar)
			integer, intent(in):: ivtext, ivchar
		end function !function j_line2(ivtext,ivchar)
 
		!linech1(ivtext,name,le) : as line but now the input text is a charvar or charconst
		! function j_linech1(ivtext,name,le)
		! integer, intent(in):: ivtext, le
		! character*1, intent(in) :: name(le)
		! end function
 
		!puth(iv,iel,ival) : putting value ival into element iel the integer fork of iv
		! subroutine puth(iv,iel,ival)
		! use getmod, only: j_inci
		! integer, intent(in):: iv,iel,ival
		! end subroutine
 
		!putr(iv,iel,val) : putting value val into element iel the double fork of iv
		subroutine j_putor(iv,iel,val)
			integer, intent(in):: iv,iel
			real, intent(in):: val
		end subroutine !subroutine j_putor(iv,iel,val)
 
		subroutine j_putod(iv,iel,val)
			integer, intent(in):: iv,iel
			double precision, intent(in):: val
		end subroutine !subroutine j_putor(iv,iel,val)
 
		!putoi(iv,iel,ival) : putting value ival into element iel the integer fork h2 of iv
		subroutine j_putoi2(iv,iel,ival)
			integer, intent(in):: iv,iel,ival
		end subroutine !subroutine j_putoi2(iv,iel,ival)
 
		!putt(iv,iel,ival) : putting value ival into element iel the integer fork of iv in trna
		subroutine j_putoi(iv,iel,ival)
			integer, intent(in):: iv,iel,ival
		end subroutine !subroutine j_putoi(iv,iel,ival)
 
		subroutine j_putoizero(iv,ival)   !put ival to o(iv)%i(0) and 0 to o(iv)%i(ival+1) (j_compiler)
			integer, intent(in):: iv,ival
		end subroutine !subroutine j_putoizero(iv,ival)
 
 
 
		!incch(iv) : increase size of text fork of an object
		subroutine j_incch(iv,mins)
			integer, intent(in):: iv,mins
		end subroutine !subroutine j_incch(iv)
 
		!getsubobject(ivobject,locsubobject,iptype,ivout) :
		! subroutine j_getsubobject(ivobject,locsubobject,iptype,ivout)
 
		! integer, intent(in) :: ivobject
		! integer, intent(in) :: locsubobject
		! integer, intent(in) :: iptype
		! integer, intent(out):: ivout
		! end subroutine
 
		!closecommandbuf(ivcommandbuf) :
		! subroutine j_closecommandbuf(ivcommandbuf)
		! integer, intent(in) ::ivcommandbuf
		! end subroutine
 
		!getchar2(iv,buffer,le) : get char constant or char varaible
		subroutine j_getchar2(iv,buffer,le)
			integer, intent(in):: iv
			integer, intent(out):: le
			character*(*), intent(out) :: buffer
		end subroutine !subroutine j_getchar2(iv,buffer,le)
 
		subroutine j_getchar3(iv,buffer,le,ext) !get character
			integer, intent(in),optional:: iv
			integer, intent(out):: le
			character*(*), intent(out) :: buffer
			character*(*),intent(in),optional::ext
		end subroutine j_getchar3
 
		!inch(iv) : increase size of the integer fork of an object
		subroutine j_inci(iv,mins)
			integer, intent(in):: iv,mins
		end subroutine !subroutine j_inci(iv)
 
		subroutine j_inctxt(iv,mins)
			integer, intent(in):: iv,mins
		end subroutine !subroutine j_inci(iv)
 
		!incr(iv) : increase size of the real fork of an object
		subroutine j_incr(iv,mins)
			integer, intent(in):: iv,mins
		end subroutine !subroutine j_incr(iv)
 
		!inch2(iv) : increase size of the 2. integer fork of an object
		subroutine j_inci2(iv,mins)
			integer, intent(in):: iv,mins
		end subroutine !subroutine j_inci2(iv)
 
		function j_isin(text,vector,n)
			integer, intent(in):: n
			character*(*), intent(in)::  text, vector(n)
		end function !function j_isin(text,vector,n)
 
		!clean(text,le) ! remove blnaks etc ,  le is the length of the cleaned text
		subroutine j_clean(text,le) ! remove blanks, tabs etc , le is the length of the cleaned text
			integer, intent(out):: le
			character (len=*),intent(inout):: text
		end subroutine !subroutine j_clean(text,le)
 
		subroutine j_cleanstart(text,le) ! remove blanks, tabs etc , le is the length of the cleaned text
			integer, intent(out):: le
			character (len=*),intent(inout):: text
		end subroutine !subroutine j_clean(text,le)
 
		subroutine j_differ(list1,n1,list2,n2,list3,n3) !picks from list1 elements which are not in list2 to list3
			integer, intent(in):: n1, n2
			integer, intent(out):: n3
			integer, intent(in):: list1(*),list2(*)
			integer, intent(out):: list3(*)
		end subroutine !subroutine j_differ(list1,n1,list2,n2,list3,n3)
 
		function j_ndiffer(list1,n1,list2,n2) !number of elements of list1 which are not in list2
			integer, intent(in):: n1,n2
			integer, intent(in):: list1(*),list2(*)
		end function !function j_ndiffer(list1,n1,list2,n2)
 
		subroutine j_union(list1,n1,list2,n2,list3,n3) !the union of !!list1 and list2 put to list3
			integer, intent(in):: n1,n2
			integer, intent(in):: list1(*),list2(*)
			integer, intent(out):: list3(*)
			integer, intent(out):: n3
		end subroutine !subroutine j_union(list1,n1,list2,n2,list3,n3)
 
		function j_nunion(list1,n1,list2,n2) !size of union of !!list1 and list2
			integer, intent(in):: n1,n2
			integer, intent(in):: list1(*),list2(*)
		end function !function j_nunion(list1,n1,list2,n2)
 
		subroutine  j_uniondif(list1,n1,list2,n2,list3,n3,list4,n4) !!!!list1+list2-list3
			integer, intent(in):: n1,n2,n3
			integer, intent(out):: n4
			integer, intent(in):: list1(*),list2(*),list3(*)
			integer, intent(out):: list4(*)
		end subroutine !subroutine  j_uniondif(list1,n1,list2,n2,list3,n3,list4,n4)
 
		subroutine j_msd2(nvar,x,xm,ss,wt,sumwt,summa)
			integer, intent(in):: nvar
			double precision, intent(in):: wt
			double precision, intent(out):: xm(nvar),ss(nvar),sumwt
			double precision, intent(in):: x(nvar),summa(nvar)
		end subroutine !subroutine j_msd2(nvar,x,xm,ss,wt,sumwt)
 
		subroutine j_msd21(x,xm,ss,wt,sumwt,summa)
			double precision, intent(in):: wt
			double precision, intent(out):: xm,ss,sumwt
			double precision, optional,intent(out):: summa
			double precision, intent(in):: x
		end subroutine !subroutine j_msd2(nvar,x,xm,ss,wt,sumwt)
 
		function j_nonblank(inp,ial,lop)
			integer, intent(in):: ial,lop
			character*(*), intent(in):: inp
		end function !function j_nonblank(inp,ial,lop)
 
		function j_lastblank(inp,ial,lop)
			integer, intent(in):: ial,lop
			character*(*), intent(in):: inp
		end function !function j_lastblank(inp,ial,lop)
 
		!jreps2(inp,i1,i2,lop,aa) : replaces segement i1:ii2 with the character value of aa
		subroutine j_reps2(inp,i1,i2,lop,aa)
			integer, intent(in):: i1,i2
			integer, intent(out):: lop
			double precision, intent(in):: aa
			character*(*), intent(inout):: inp
		end subroutine !subroutine j_reps2(inp,i1,i2,lop,aa)
 
		!jrepse(inp,i1,i2,lop,iii) :  replace segement i1:i2 by shortest presentation of integer iii
		subroutine j_repse(inp,i1,i2,lop,iii,ial2)
			integer, intent(in):: i1,i2,iii
			integer, intent(inout):: lop
			character*(*), intent(inout):: inp
			integer,optional,intent(out)::ial2
		end subroutine !subroutine j_repse(inp,i1,i2,lop,iii)
 
		!jrepl(jono1,i1,i2,linp,jono2,le2) : replaces the substring jono1(i1:i2) by string jono2(1:le2)
		subroutine j_repl(jono1,i1,i2,linp,jono2,le2)
			integer, intent(in):: i1,i2,le2
			integer, intent(inout):: linp
			character*(*), intent(inout):: jono1
			character*(*), intent(in):: jono2
		end subroutine !subroutine j_repl(jono1,i1,i2,linp,jono2,le2)
 
		!chr8(a) : Returns real value as a character*8 variable.
 
 
		!chr8b(a,le): Returns real value as a character*8 variable.
		character*8 function j_chr8b(a,le)
			real, intent(in):: a
			integer, intent(out):: le
		end function !character*8 function j_chr8b(a,le)
 
		!nextlim0(inp,ial,lop,limit) : like nextlim but returns 0 if limiter not found'
		function j_nextlim0(inp,ial,lop,limit)
			integer,intent(in):: ial,lop
			character*(*), intent(in):: inp, limit
		end function !function j_nextlim0(inp,ial,lop,limit)
 
		!nextlim(inp,ial,lop,limit) :  Finds the next limiter.
		function j_nextlim(inp,ial,lop,limit,inhipsu)
			character*(*), intent(in):: inp, limit
			integer, intent(in):: ial,lop
			logical,optional, intent(in):: inhipsu
		end function !function j_nextlim(inp,ial,lop,limit)
 
		function j_prevlim(inp,ial,limit)
			character*(*), intent(in):: inp, limit
			integer, intent(in):: ial
		end function !function j_nextlim(inp,ial,lop,limit)
 
		!nextlim(inp,ial,lop,limit) :  Finds the next limiter.
		function j_nextlim2(inp,ial,lop,limit)
			character*(*), intent(in):: inp, limit
			integer, intent(in):: ial,lop
		end function !function j_nextlim2(inp,ial,lop,limit)
 
		function j_nextlimset(inp,ial,lop,limset)
			character*(*), intent(in):: inp, limset
			integer, intent(in):: ial,lop
		end function !function j_nextlim2(inp,ial,lop,limit)
 
		function j_nextword(inp,ial,lop,limit)
			character*(*), intent(in):: inp
			integer, intent(in):: ial,lop
			character*(*),intent(in):: limit
		end function !function j_nextword(inp,ial,lop,limit)
 
		subroutine j_jreplace(inp,ial,lop,cout,lcout,word1,lword1,word2,lword2)
			character*(*), intent(in):: inp,word1,word2
			character*(*), intent(out):: cout
			integer, intent(in):: ial, lop,lword1,lword2
			integer, intent(out):: lcout
		end subroutine !subroutine j_jreplace(inp,ial,lop,cout,lcout,word1,lword1,word2,lword2)
 
		!nextrp(inp,ial,lop) : Finds the next balanced parenthesis,including []
		function j_nextrp(inp,ial,lop)
			character*(*), intent(in):: inp
			integer, intent(in):: ial,lop
		end function !function j_nextrp(inp,ial,lop)
 
		!adjul2(inp) : Adjusts a character variable to the left, i.e. removes initial blanks
		subroutine j_adjul2(inp)
			character*(*), intent(inout):: inp
		end subroutine !subroutine j_adjul2(inp)
 
		!chi5(i,il) : Returns integer i as character*5
		character*5 function j_chi5(i,il)
			integer, intent(in):: i,il
		end function !character*5 function j_chi5(i,il)
 
		double precision function j_cotan(x)  !!!math cotan not available in all systems
			double precision,intent(in)::x
		end function !real function j_cotan(x)
 
		!chr10 : Returns double precision a as character*10 variable
		character*10 function j_chr10(a)
			DOUBLE PRECISION, intent(in)::  a
		end function !character*10 function j_chr10(a)
 
		subroutine j_gayainit(iob,io)
			integer, intent(in):: iob,io
		end subroutine !subroutine j_gayainit(iob)
 
		subroutine j_gayax(nuown2,is)   !,readxl,v)
			integer, intent(in):: nuown2  ! unit used for reading
			!	integer, intent(in):: readxl(0:*)  !this is the variable list given in subread-> option
			integer, intent(in):: is  ! the number of schedule we are now reading
			!	real, dimension(*), intent(out):: v ! read varaibles should be put into v so that
		end subroutine !subroutine j_gayax(nuown2,is,readxl,v)
 
		subroutine j_f_pca_gaya(x,ix,p,ipp)
			real, dimension(*), intent(out):: x
			real, dimension(*), intent(in):: p
			integer, intent(out):: ix
			integer, intent(in):: ipp
		end subroutine !subroutine j_f_pca_gaya(x,ix,p,ipp)
 
		!rlinter : linear interpolation
		double precision function j_rlinter(x0,x1,y0,y1,x)
			double precision, intent(in):: x0,x1,y0,y1,x
		end function !real function j_rlinter(x0,x1,y0,y1,x)
 
		double precision function j_bilin(xa,xy,za,zy,aa,ay,ya,yy,x,z)
			double precision,intent(in):: xa,xy,za,zy,aa,ay,ya,yy,x,z
		end function
		!real function j_bilin(xa,xy,za,zy,aa,ay,ya,yy,x,z)
		double precision function j_interplane(xi0,xi1,xi2,yi0,yi1,yi2,zi0,zi1,zi2,xa,ya)
			!used to interpolate covariance matrix near digonal
			double precision, intent(in)::xi0,xi1,xi2,yi0,yi1,yi2,zi0,zi1,zi2
			double precision, intent(in)::xa,ya
		end function
 
		double precision function j_sqrtt(x)
			double precision, intent(in):: x
		end function !real function j_sqrtt(x)
 
		!flini(np,x,y,r1,r2) :  numeric linear integration
		double precision function j_flini(np,x,y,r1,r2)
			integer, intent(in):: np
			double precision, intent(in):: r1,r2
			double precision, dimension(*), intent(in):: x,y
		end function !real function j_flini(np,x,y,r1,r2)
 
		!20150812(arg1<->arg2) oli: 		!deffig(name,iv,mxfigs,mxpoints,xmin,ymin,xmax,ymax,dx,dy,title,ivout)defines a figure object
		!deffig(iv,name,mxfigs,mxpoints,xmin,ymin,xmax,ymax,dx,dy,title,ivout)defines a figure object
		!20150812(arg1<->arg2) oli: 		subroutine deffig(name,iv,mxfigs,mxpoints,xmin,ymin,xmax,ymax,dx,dy,title,ivout)
		subroutine j_deffig(iv,name,mxfigs,mxpoints,xmin,ymin,xmax,ymax,dx,dy,title,ivout)
			integer, intent(in):: iv, mxfigs, mxpoints
			real, intent(in):: xmin,ymin,xmax,ymax,dx,dy
			integer, intent(out):: ivout
			character(len=*), intent(in):: name, title
		end subroutine !subroutine j_deffig(iv,name,mxfigs,mxpoints,xmin,ymin,xmax,ymax,dx,dy,title,ivout)
 
		! subroutine j_deffig2(iv)
		! integer,intent(in)::iv
		! end subroutine !subroutine j_deffi
 
 
 
 
		subroutine j_showfig(ivfig)
			integer,intent(in)::ivfig
 
 
		end subroutine !subroutine j_showfig(ivout,p,iob)
 
		function j_font(label,le)
			character*(*),intent(in)::label
			integer,intent(in)::le
		end function
 
		subroutine j_putfig(text)
			character*(*),intent(in)::text
		end subroutine
 
		subroutine j_putfig0(line,text)
			integer,intent(in)::line
			character*(*),intent(in)::text
		end subroutine
		subroutine j_putfigbreak(nu)
			integer,intent(in),optional::nu
		end subroutine
 
		subroutine j_putfigxy(x,y,se,nu)
			double precision,intent(in)::x,y
			double precision, intent(in),optional::se
			integer,intent(in),optional::nu
		end subroutine
 
		subroutine j_gpplot(text,add)
			character*(*),intent(in)::text
			logical,intent(in),optional::add
		end subroutine
 
 
 
		subroutine j_replacefig(line,text)
			integer,intent(in)::line
			character*(*),intent(in)::text
		end subroutine
 
		!openread(fil,for,nu) : open file for reading
		subroutine j_openread(fil,for,nu,irecl)  ! open file for reading (!!io)
			character*(*), intent(in):: fil,for
			integer, intent(out) :: nu
			integer,optional, intent(in) ::irecl
		end subroutine !subroutine j_openread(fil,for,nu)
 
 
 
		!openreadiv(iv,for,nu) : get file for reading when file name is stored in character constant iv
		subroutine j_openreadiv(iv,for,nu,ext,irecl)
			integer, intent(in) :: iv
			integer, intent(out) :: nu
			character*(*), intent(in):: for
			character*(*),optional,intent(in)::ext
			integer,intent(in),optional::irecl
		end subroutine !subroutine j_openreadiv(iv,for,nu)
 
		!getwritefile(ivfile,ivform,bin) : get file for reading, name is stored in character constant iv
		subroutine j_getwritefile(ivfile,ivform,bin,del)
			integer, intent(in):: ivfile, ivform
			logical, intent(out):: bin
			!			character*(*), intent(in),optional:: ext
			logical,intent(in),optional::del
		end subroutine !subroutine j_getwritefile(ivfile,ivform,bin)
 
		subroutine j_getfile(nu,rw,ivfile,ivform,forma,ext,replace,irecl,ivout,readit,deleted) !get file for writing, name is stored in character constant ivfile
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
		end subroutine
 
		subroutine j_fromutf8(line)
			character*(*),intent(inout)::line
		end subroutine
		subroutine j_toutf8(line,le)
			character*(*),intent(inout)::line
			integer,intent(inout)::le
		end subroutine
 
		logical function j_exist(ivfile,ext)
			integer, intent(in)::ivfile
			character*(*), intent(in),optional::ext
		end function j_exist
 
		!getwritefilebin(ivfile) : get file for reading, name is stored in character constant iv
		subroutine j_getwritefilebin(ivfile,ext,irecl,ivout)
			integer, intent(in):: ivfile
			character*(*), intent(in),optional::ext
			integer, intent(in),optional::irecl
			integer,intent(out),optional::ivout
		end subroutine !subroutine j_getwritefilebin(ivfile)
 
		!clear options
		subroutine j_clearoption(iob,io)
			integer,intent(in)::iob,io
		end subroutine !subroutine j_clearopt()
 
		!check if there are options not checked by getoption
		!		subroutine j_clearopt2()
		!		end subroutine !subroutine j_clearopt2()
 
		!igetopt(iob,mopt) : -1 ==not  0== opt->,  otherwise iv for first argument
		function j_igetopt(iob,io,mopt) ! %option obsolete
			integer, intent(in):: iob,io, mopt
		end function !function j_igetopt(iob,mopt)
 
		logical function j_isopt(iob,io,mopt,default) ! %o
			integer, intent(in):: iob,io, mopt
			logical, intent(in),optional:: default
		end function !function j_igetopt(iob,mopt)
 
 
		!igetoptval(iob,mopt,idef,idef0) : -1 ==not  0== opt->,  otherwise iv for first argument
		function j_igetoptval(iob,io,mopt,idef,idef0)
			integer, intent(in):: iob,io,mopt,idef,idef0
		end function !function j_igetoptval(iob,mopt,idef,idef0)
 
		!igetoptout(iob,mopt) : -1 ==not  0== opt->,  otherwise iv for first argument
		! function j_igetoptout(iob,io,mopt)
		! integer, intent(in):: iob,io, mopt
		! end function !function j_igetoptout(iob,mopt)
 
		!igetopt2(iob,io,mopt,itype) : -1 ==not  0== opt->,  otherwise iv for first argument
		! function j_igetopt2(iob,io,mopt,itype)
		! integer, intent(in):: iob,io, mopt
		! integer, intent(out):: itype
		! end function !function j_igetopt2(iob,io,mopt,itype)
 
		!put values for option variables
		subroutine j_putoptv(iob,li,j,name,iv,value)
			integer, intent(in):: iob,li,j,iv
			character*(*), intent(in):: name
			double precision value
		end subroutine !subroutine j_putoptv(iob,li,j,name,iv,value)
 
		!igetoptv(igeto,name,iv,single,ivout) : get output option varaible e.g. stat(mean->
		subroutine j_igetoptv(igeto,name,iv,single,ivout)
			integer, intent(in):: igeto	, iv
			character*(*), intent(in):: name
			logical, intent(in):: single
			integer, intent(out):: ivout
		end subroutine !subroutine j_igetoptv(igeto,name,iv,single,ivout)
 
		!getdataobjects(iob) : initilization for data-> option
		! subroutine j_getdataobjects(iob)
		! integer, intent(in):: iob
		! end subroutine !subroutine j_getdataobjects(iob)
 
		! subroutine j_alldatavars(vars,nvar) !all variables in data +trans variables
		! ! if trans variables are already in data there will be space for them in vars
		! ! but they are not stored twice
		! integer,dimension(:),allocatable,intent(inout) :: vars
		! integer,intent(out) ::nvar
		! end subroutine
 
		!nob(ivdata) : number of obs in data
		function j_nobs_data(ivdata)
			integer, intent(in):: ivdata
		end function !function j_nobs_data(ivdata)
 
 
		subroutine j_getdots(i1,name2,n)
			integer, intent(in)	:: i1
			character*(*),intent(in)::name2
			integer, intent(out):: n
		end subroutine !subroutine j_getdots(i1,i2,new,list,n)
 
		real function j_valuesspl(ifunc,arg) !compute the value of a smoothing spline
			integer,intent(in):: ifunc
			real, intent(in)::arg
		end function !real function j_valuesspl(ifunc,arg)
 
		subroutine j_zerondo()
		end subroutine !subroutine j_zerondo()
 
		!jcompil(input,ivteku,oneline,newin,ivinl,ivoutl,matrix,localin,localout,jhaka) : trasnforamtion interpreter
 
 
		subroutine j_tracecheck(iv)
			integer, intent(in):: iv
		end subroutine !subroutine j_tracecheck(iv)
 
		integer function j_ibittest(ifunc,irow,icol)
			integer,intent(in) ::ifunc
			integer,intent(in) ::irow
			integer,intent(in) ::icol
		end function !integer function j_ibittest(ifunc,irow,icol)
 
		subroutine j_nextop(inp,icur,last,oper,noper,ipos,ipos1,ioper)
			character*(*), intent(in):: inp,oper(*)
			integer, intent(inout):: icur
			integer, intent(in):: last, noper
			integer, intent(out):: ipos,ipos1,ioper
		end subroutine !subroutine j_nextop(inp,icur,last,oper,noper,ipos,ipos1,ioper)
 
 
		integer function j_nargopt(iob,io,mopt)
			integer,intent(in) ::iob,io
			integer, intent(in)::mopt
		end function !integer function j_nargopt(iob,mopt)
 
		subroutine j_deflistobject(iv,name,ivout,list0,list,listold,nres,expand,ivin,ilist)
			integer, intent(in):: iv
			character*(*), intent(in):: name
			integer, intent(out):: ivout
			integer,optional, intent(in):: list0
			integer,optional,dimension(:), intent(in):: list
			integer,optional,dimension(:), intent(in):: listold
			integer,optional, intent(in):: nres
			logical,optional,intent(in)::expand
			integer,optional,intent(in)::ivin
			logical,optional,intent(in)::ilist !make ilist object
		end subroutine
 
		! subroutine j_deflist(iv,name,list0,ivout,nres)
		! character*(*), intent(in):: name
		! integer, intent(in):: iv
		! integer, intent(in):: list0
		! integer, intent(out):: ivout
		! integer, optional,intent(in)::nres   !extra
		! end subroutine !subroutine j_deflist2(iv,name,list,ivout)
 
		! subroutine j_deflist2(iv,name,list,ivout)
		! !20150812(arg!subroutine deflist2(name,iv,list,ivout) : define a list object, and put list to it
		! character*(*), intent(in):: name
		! integer, intent(in):: iv
		! integer, intent(in):: list(0:*)
		! integer, intent(out):: ivout
		! end subroutine !subroutine j_deflist2(iv,name,list,ivout)
 
		! !deflist3(iv,name,list0,ivout) : allocates list object with size list0, but put it as empty!20150812(arg1<->arg2) oli: 		subroutine deflist3(name,iv,list0,i
		! subroutine j_deflist3(iv,name,list0,ivout,list2)
		! character*(*), intent(in):: name
		! integer, intent(in):: iv
		! integer, intent(in):: list0
		! integer, intent(out):: ivout
		! logical,intent(in),optional::list2
		! end subroutine !subroutine j_deflist3(iv,name,list0,ivout)
 
		subroutine j_deflistobjectinp(iv,name,inp,ivout) !!!list
			!makes a list from character variable inp which contains the names of objects separated with commas			character*(*), intent(in):: name
 
			character*(*), intent(in):: inp,name
			integer, intent(in):: iv
			integer, intent(out):: ivout
		end subroutine j_deflistobjectinp
		! !deflistopt(iv,name,list0,list,ivout) : allocates list object with size list0, and put list (strating from 1)!20150812(arg1<->arg2) oli:
		! subroutine j_deflistopt(iv,name,list0,list,ivout)
		! character*(*), intent(in):: name
		! integer, intent(in):: list(1:list0)
		! integer, intent(in):: iv,list0
		! integer, intent(out):: ivout
		! end subroutine !subroutine j_deflistopt(iv,name,list0,list,ivout)
 
		! !defmergelist(name,iv,list,list2,single,ivout) : merging two lists+obj
		! subroutine j_defmergelist(iv,name,list,list2,single,ivout)
		! integer, intent(in):: iv
		! character*(*), intent(in):: name
		! integer, intent(in):: single
		! integer, intent(in):: list(0:*)  !starts from io
		! integer, intent(in):: list2(0:*)  !starts from io
		! integer, intent(out):: ivout
		! end subroutine !subroutine j_defmergelist(iv,name,list,list2,single,ivout)
		logical function j_isnumber(ch) ! is letter ?
			character*3, intent(in):: ch
		end function j_isnumber !logical function j_isletter(ch)
 
 
		logical function j_isletter(ch)
			character*1, intent(in):: ch
		end function !logical function j_isletter(ch)
 
		!istrans(iv) : is iv a transformation
		logical function j_istrans(iv)
			integer, intent(in):: iv
		end function !logical function j_istrans(iv)
 
		integer function j_outputlist(ivtrans) !outputlist of transformation set ivtrans
			integer,intent(in):: ivtrans
		end function !integer function j_outputlist(ivtrans)
 
		!vname(iv) : name of object iv
		character*24 function j_vname(iv) ! name of object iv
			integer, intent(in):: iv
		end function !character*24 function j_vname(iv)
 
		function j_lename(iv)
			integer, intent(in):: iv
		end function !function j_lename(iv)
 
		!lenlist(iob) : length of list, -1 if not a list
		function j_lenlist(iob) ! length of list, -1 if not a list
			integer, intent(in):: iob
		end function !function j_lenlist(iob)
 
		!inlist(i,list) : is i in list NO=>0
		function j_inlist(i,list0,list)
			integer, intent(in):: i
			integer,dimension(:), intent(in):: list
		end function !function j_inlist(i,list)
 
		! subroutine j_crash()
		! end subroutine !subroutine j_crash()
 
		integer function j_nextio(iob,io)
			integer, intent(in):: iob,io
		end function
 
		function j_inlistobject(i,ivlist)
			integer, intent(in):: i,ivlist
		end function !function j_inlist2(i,ivlist
 
		!inlist2(i,ivlist) : is i in a list object
		! function j_inlist2(i,ivlist)
		! integer, intent(in):: i,ivlist
		! end function !function j_inlist2(i,ivlist)
 
		! !inlist2b(i,ivlist) : as inlist2, but if(ivlist.le.0) --> error
		! function j_inlist2b(i,ivlist)
		! integer, intent(in):: i,ivlist
		! end function !function j_inlist2b(i,ivlist)
 
		!inlist3(i,list,list0) : is i in list, length given in list0 , not in list(0)
		function j_inlist1(i,list0,list)
			integer, intent(in):: i, list0
			integer, intent(in):: list(list0)
		end function !function j_inlist3(i,list,list0)
 
		!putlist0(i,list) : put i into list, no bound checking
		function j_putlistobject(ivlist,single,list0,list,ivin,ignored,append)
			integer,intent(in)::ivlist
			integer,optional,intent(in)::single
			integer,optional,intent(in)::list0
			integer,dimension(:),optional,target,intent(in)::list
			integer,optional,intent(in)::ivin  !list object
			logical,optional,intent(in)::ignored  !list object
			logical,optional,intent(in)::append !list object
		end function
 
		! function j_putlist0(i,list)
		! integer, intent(in):: i
		! integer, intent(inout):: list(0:*)
		! end function !function j_putlist(i,list)
 
		!putlist(i,list) : put i into list, no bound checking
		function j_putlist(i,list)
			integer, intent(in):: i
			integer, dimension(:),allocatable, intent(inout):: list  !list(0:*)
			!			integer, intent(inout):: list(0:*)
		end function !function j_putlist(i,list)
 
 
		! !putlist2(i,ivlist) : put i into list object
		! function j_putlist2(i,ivlist)
		! integer, intent(in):: i,ivlist
		! end function !function j_putlist2(i,ivlist)
 
		!putlist2plus(i,ivlist) : put i into list object
		! function j_putlist2plus(i,ivlist)
		! integer, intent(in):: i,ivlist
		! end function !function j_putlist2plus(i,ivlist)
 
		!putlist2b(i,ivlist) : put i into list object, if i is list expand it, and put all
		! function j_putlist2b(i,ivlist)
 
		! integer, intent(in):: i,ivlist
		! end function !function j_putlist2b(i,ivlist)
 
		!putlist3(i,ivlist) : append i into list object
		! function j_putlist3(i,ivlist)
 
 
		! integer, intent(in):: i,ivlist
		! end function !function j_putlist3(i,ivlist)
 
		!putinput(iv,ivinl,ivoutl) : put variable into inputlist if not in the outputlist
		subroutine j_putinput(iv,ivinl,ivoutl,ivarg)
 
 
			integer, intent(in):: iv,ivinl,ivoutl,ivarg
		end subroutine !subroutine j_putinput(iv,ivinl,ivoutl)
 
		!putoutput(iv,ivinl,ivoutl) : put varaible in the outputlist, ignore $-varaibles
		subroutine j_putoutput(iv,ivinl,ivoutl,ivarg)
			integer, intent(in):: iv,ivinl,ivoutl,ivarg
		end subroutine !subroutine j_putoutput(iv,ivinl,ivoutl)
 
		!xt(ivmat,ivkeep,iobs) : get all keep-variables for observation iobs for data matrix ivmat
		subroutine j_xt(ivmat,ivkeep,iobs)
			integer, intent(in):: ivmat,ivkeep,iobs
		end subroutine !subroutine j_xt(ivmat,ivkeep,iobs)
 
		subroutine j_printsource(iob,io)
			integer,intent(in)::iob,io
		end subroutine
 
 
		!ivoutputlistivtrans) : gives the outputlist; if there is no then return zero
 
 
		subroutine j_getdataobject(iob,io,ivdata)  ! initilization for data-> option  %%data
			integer,intent(in)::iob,io
			integer,intent(in),optional::ivdata
			! logical,intent(in),optional::isup
			! integer,intent(in),optional::level0
		end subroutine
 
 
		subroutine j_linkdata(ivup,ivsub)
			integer, intent(in)::ivup,ivsub
		end subroutine
 
		function j_datalevels(ivdata)
			integer,intent(in)::ivdata
		end function
 
		function j_bottomdata(ivdata)
			integer,intent(in)::ivdata
		end function
 
		function j_topdata(ivdata)
			integer,intent(in)::ivdata
		end function
 
		subroutine j_getobs(iobs)
			integer*8,intent(in)::iobs
		end subroutine
 
		subroutine j_getobsup(iobs)
			integer,intent(in)::iobs
		end subroutine
 
		subroutine j_getnobswtot(ivdata)
			integer,intent(in)::ivdata
		end subroutine
 
 
 
		subroutine j_getdataobject2(iob,io)  ! initilization for data-> option  %%data
			integer,intent(in)::iob,io
		end subroutine
 
		subroutine j_getobs2(iobs)
			integer,intent(in)::iobs
		end subroutine
		subroutine j_getobs20()
 
		end subroutine
 
		subroutine j_getobswup(iobs,ivdata)
			integer,intent(in)::iobs,ivdata
		end subroutine
 
 
		subroutine j_nextobs()
 
		end subroutine !subroutine j_nextobs()
 
		subroutine j_getobs0(ivdata,iobs)  !get observation iob in dta set ivdta (upper levels not used)
			integer, intent(in):: ivdata !data object
			integer*8, intent(in) ::iobs
		end subroutine !subroutine j_getob(ivdata,iob)
 
 
		!ipc(iv) : is ic character, YES=>1 NO=>0
		function j_ipc(iv)
			integer, intent(in):: iv
		end function j_ipc !function j_ipc(iv)
 
		subroutine j_andlist(list1,list2,olist)
			integer, intent(in):: list1(0:*),list2(0:*)
			integer, intent(out):: olist(0:*)
		end subroutine !subroutine j_andlist(list1,list2,olist)
 
		subroutine j_stopj()
		end subroutine !subroutine j_stopj()
 
		subroutine j_closeunit(nu)
			integer, intent(in):: nu
		end subroutine !subroutine j_closeunit(nu)
 
		!20150812(arg1<->arg2) oli: 		!defchar(name,iv,ivout) : define a character variable
		!defchar(iv,name,ivout) : define a character variable
		!20150812(arg1<->arg2) oli: 		subroutine defchar(name,iv,ivout)
		subroutine j_defchar(iv,name,ivout)  ! !!char
			character(len=*), intent(in):: name
			integer, intent(in):: iv
			integer, intent(out):: ivout
		end subroutine !subroutine j_defchar(iv,name,ivout)
 
 
 
 
 
		logical function j_ischarconst(iv)  ! !!char
			integer, intent(in):: iv
		end function !logical function j_ischarconst(iv)
 
		!isconst(name) : 0 =is not, -1 looks like but is not
		function j_isconst(name)
			character*(*), intent(in):: name
			!	logical, optional,intent(in)::silent
		end function !function j_isconst(name)
 
		function j_getconst(name)
			character*(*), intent(in):: name
			!	logical, optional,intent(in)::silent
		end function !function j_isconst(name)
 
		function j_num2iv(r)
			double precision, intent(in):: r
		end function !function j_isconst(name)
 
		!printuntil(nu,inp,le,tag) : print file until tag, but not it
		subroutine j_printuntil(nu,inp,le,tag)
			integer, intent(in):: nu
			character*(*), intent(in):: tag
			character*(*), intent(out):: inp
			integer, intent(out):: le
		end subroutine !subroutine j_printuntil(nu,inp,le,tag)
 
		subroutine j_readuntil(nu,inp,le,tag)
			integer, intent(in):: nu
			character*(*), intent(in):: inp,tag
			integer, intent(out):: le
		end subroutine !subroutine j_readuntil(nu,inp,le,tag)
 
		!isanyin(vector1,nvector1, vector2,nvector2, cvector1, cvector2) : Testing if any element of text-vector text is in text-vector vector
		subroutine j_isanyin(vector1,nvector1, vector2,nvector2, cvector1, cvector2)
			character*(*), intent(in):: vector1(nvector1), vector2(nvector2), cvector1, cvector2
			integer, intent(in):: nvector1, nvector2
		end subroutine !subroutine j_isanyin(vector1,nvector1, vector2,nvector2, cvector1, cvector2)
 
		function j_iopts(opt)
			character*(*), intent(in):: opt
		end function !function j_iopts(opt)
 
		subroutine j_getvalues(ix,value9,nval,iz,value2,nval2)
			integer, intent(in):: ix, iz
			double precision, dimension(*), intent(out):: value9,value2
			integer*8, intent(out):: nval, nval2
		end subroutine !subroutine j_getvalues(ix,value9,nval,iz,value2,nval2)
 
		subroutine j_bitset(ifunc,irow,icol,val)
			integer, intent(in):: ifunc,irow,icol
			real, intent(in):: val
		end subroutine !subroutine j_bitset(ifunc,irow,icol,val)
 
		! subroutine j_mergelist(list,n)
		! integer,dimension(*):: list
		! integer n
		! end subroutine
 
		double precision function j_getmatel(ivmat,irow,icol) !subroutine
			integer, intent(in):: ivmat,irow,icol
		end function !double precsion function j_getmatel(ivmat,irow,icol)
 
 
 
		!asschar2(ivin,ivout) : assign character variable ?
		subroutine j_asschar2(ivin,ivout)
			integer, intent(in):: ivin,ivout
		end subroutine !subroutine j_asschar2(ivin,ivout)
 
		!getinput(prompt,inprint) : gets next input line
		subroutine j_getinput(prompt,inprint,nul0t,single)
			character*(*), intent(in):: prompt
			integer, intent(in),optional:: inprint
			integer,intent(in),optional :: nul0t !at what value of nul(0) returns
			logical,intent(in),optional::single
		end subroutine !subroutine j_getinput(prompt,inprint,nul0t)
 
		subroutine j_sit()
 
		endsubroutine
 
 
		!20150812(arg1<->arg2) oli: 		!defconst(varname,ivin,text) compute the numeric value of a text string
		!defconst(ivin,varname,text) compute the numeric value of a text string
		!20150812(arg1<->arg2) oli: 		subroutine defconst(varname,ivin,text)
		subroutine j_defconst(ivin,varname,text)
			character (len=*), intent(in):: text,varname
			integer, intent(in):: ivin
		end subroutine !subroutine j_defconst(ivin,varname,text)
 
		!val(text) : subroutine compute the numeric value of a text string, used in getinput
		double precision function j_val(text)
			character (len=*), intent(in):: text
		end function !real function j_val(text)
 
		integer function j_tex2iv(text,isplus)
			character (len=*), intent(in):: text
			integer,intent(out)::isplus
		end function !real function j_val(text)
 
		double precision function j_iv2val(iv)
			integer,intent(in)::iv
		end function !real function j_val(text)
 
		!	character(len=lenoption) function option_name(iopt)
		function j_option_name(iopt,le)
			integer,intent(in)::iopt
			integer, intent(in):: le
			character(len=le) j_option_name
		end function j_option_name !function j_option_name(iopt,le)
 
		function j_function_name(ifunc,le)
			integer, intent(in) :: ifunc
			integer, intent(in) :: le
			character(len=le) j_function_name
		end function j_function_name !function j_function_name(ifunc,le)
 
		function j_object_name(iv,le)
			integer, intent(in) :: iv
			integer, intent(in) :: le
			character(len=le) j_object_name
		end function j_object_name !function j_object_name(iv,le)
 
		function j_objecttype_name(itype,le)
			integer, intent(in) :: itype
			integer, intent(in) :: le
			character(len=le) j_objecttype_name
		end function j_objecttype_name !function j_objecttype_name(itype,le)
 
		! function j_subobjecttype_name(itype,le)
		! integer, intent(in) :: itype
		! integer, intent(in) :: le
		! character(len=le) j_subobjecttype_name
		! end function j_subobjecttype_name !function j_subobjecttype_name(itype,le)
 
		integer function j_function_index(func) ! !!function get function index
			character*(*), intent(in)::  func
		end function !integer function j_function_index(func)
 
		function j_objecttype_index(objecttype)
			character(len=*), intent(in):: objecttype
		end function !function j_objecttype_index(objecttype)
 
 
 
		integer function j_linkoption(iob,io,mopt,clear,link)
			integer,intent(in)::iob,io,mopt
			logical,optional,intent(in)::clear,link
		end function
 
		integer function j_intloc(ivec,lenvec,i)
			integer,dimension(*),intent(in):: ivec
			integer,intent(in) ::lenvec,i
		end function
 
		integer function j_codelink(iob,io,jmcode)
			integer, intent(in):: iob,io, jmcode
		end function !function j_objecttype_index(objecttype)
 
		double precision function j_codevalue(iob,link)
			integer,intent(in)::iob,link
		end function
 
 
	end interface !interface
 
	interface j_chr
		function j_chr_integer(ival,le,left,fill,le2)
			integer, intent(in) :: ival
			integer, intent(in) :: le
			character(len=le) j_chr_integer
			logical, intent(in) :: left
			logical, intent(in) ::fill
			integer, intent(out)::le2
		end function j_chr_integer !function j_chr_integer(ival,le,left,fill,le2)
 
		function j_chr_real(rval,le,left,fill,le2)
			real, intent(in) :: rval
			integer, intent(in) :: le
			character(len=le) j_chr_real
			logical, intent(in) :: left
			logical, intent(in) ::fill
			integer, intent(out)::le2
		end function j_chr_real !function j_chr_real(rval,le,left,fill,le2)
 
		function j_chr_double(dval,le,left,fill,le2)
			double precision, intent(in) :: dval
			integer, intent(in) :: le
			character(len=le) j_chr_double
			logical, intent(in) :: left
			logical, intent(in) ::fill
			integer, intent(out)::le2
		end function j_chr_double !function j_chr_double(dval,le,left,fill,le2)
	end interface j_chr !interface j_chr
 
	interface
		real function j_vlog(iff)
			logical iff
			! if(iff)then;vlog=1.;else;vlog=0.;end if
		end function j_vlog !real function j_vlog(iff)
 
 
		subroutine j_getsum(inp,linp,plus)
			integer, intent(inout) :: linp
			character*(*),intent(inout):: inp
			logical, intent(in)::plus
 
		end subroutine
 
		subroutine j_objectname(iv,name,le)
			integer,intent(in):: iv  !object index
			character*(*),intent(out) ::name  !object name
			integer,intent(out):: le  !length of the name
			!call j_getline(j_ivnames,iv,name,le)
		end subroutine
 
		subroutine j_getcurline(iob,io)
			integer,intent(in)::iob,io
		endsubroutine
 
		!20150202 real function eqf
		real function j_eqf(iv1,iv2)
			! use j_globalsmod, only: j_otype
			! use j_globalsmod, only: j_IPREAL
			! use j_globalfuncsmod, only: j_vlog
			! use j_globalsmod, only: j_v   ! real variable associated with each named object +
			! use j_globalsmod, only: j_IPCHAR
			! use j_globalsmod, only: j_tempchar
			! use j_globalsmod, only: j_tempchar2
			! use errmod, only: j_err
			integer, intent(in) ::iv1
			integer, intent(in) ::iv2
		end function j_eqf !real function j_eqf(iv1,iv2)
 
		subroutine j_printname(text1,iv,text2,iv2) !print variable name with text
			!use errmod, only: j_err
			!use j_globalsmod, only: j_namedv   ! current namber of named objects
			!use j_globalsmod, only: j_v   ! real variable associated with each named object +
			!use j_globalsmod, only: j_ivnames
			character(len=*),intent(in):: text1
			character(len=*),intent(in):: text2
			integer, intent(in)::iv
			integer, intent(in),optional:: iv2
 
		end subroutine j_printname !subroutine j_printname(text1,iv,text2,iv2)
 
		! subroutine puti(ivec,iel,ival)
		! integer,dimension(:),intent(inout), pointer::ivec    ! HHir. 8.3/2011  =>null()
		! integer,intent(in) :: iel
		! integer,intent(in) :: ival
		! end subroutine puti
 
		subroutine j_puti(ivec,iel,ival)
			integer,dimension(:),intent(inout), allocatable::ivec
			integer,intent(in) :: iel
			integer,intent(in) :: ival
		end subroutine j_puti !subroutine j_puti(ivec,iel,ival)
 
		subroutine j_checki(ivec,iel)
			integer,dimension(:),intent(inout), allocatable::ivec
			integer,intent(in) :: iel
		end subroutine j_checki!subroutine j_puti(ivec,iel,ival)
 
		! subroutine j_checkl(ivec,iel)
		! logical,dimension(:),intent(inout), allocatable::ivec
		! integer,intent(in) :: iel
		! end subroutine j_checkl!subroutine j_puti(ivec,iel,ival)
 
		subroutine j_checkd(dvec,iel,lenn)
			double precision, dimension(:),intent(inout), allocatable::dvec
			integer,intent(in) :: iel
			integer,optional,intent(out)::lenn
		end subroutine j_checkd  !subroutine j_puti(ivec,iel,ival)
 
		subroutine j_checkr(dvec,iel,lenn)
			real, dimension(:),intent(inout), allocatable::dvec
			integer,intent(in) :: iel
			integer,optional,intent(out)::lenn
		end subroutine j_checkr  !subroutine j_puti(ivec,iel,ival)
 
 
		subroutine j_putl(ivec,iel,ival)
			logical,dimension(:), intent(inout), pointer::ivec  ! HHir. 8.3/2011    =>null()
			integer :: iel
			logical ::ival
		end subroutine j_putl !subroutine j_putl(ivec,iel,ival)
 
		subroutine j_putim(ivec,iel1,iel2,ival)
			integer,dimension(:,:),intent(inout), allocatable ::ivec  ! HHir. 8.3/2011   =>null()
			integer,intent(in) :: iel1
			integer,intent(in) :: iel2
			integer,intent(in) :: ival
		end subroutine !subroutine j_putim(ivec,iel1,iel2,ival)
 
		! subroutine putv(rvec,iel,val)
		! real,dimension(:), pointer::rvec ! HHir. 8.3/2011   =>null()
		! integer,intent(in) :: iel
		! real, intent(in) ::val
		! end subroutine
 
		subroutine j_putr(rvec,iel,val,lenn)
			real,dimension(:), allocatable::rvec ! HHir. 8.3/2011   =>null()
			integer,intent(in) :: iel
			real, intent(in) ::val
			integer,optional,intent(out)::lenn
		end subroutine j_putr !subroutine j_putr(rvec,iel,val)
 
		subroutine j_putd(rvec,iel,val,lenn)
			double precision,dimension(:), allocatable::rvec ! HHir. 8.3/2011   =>null()
			integer,intent(in) :: iel
			double precision, intent(in) ::val
			integer,optional,intent(out)::lenn
		end subroutine j_putd !subroutine j_putr(rvec,iel,val)
 
		logical function j_isweak(i,nop2)
			integer,intent(in)::i
			integer, intent(in)::nop2
 
		end function !logical function j_isweak(i,nop2)
 
		RECURSIVE SUBROUTINE j_quick_sort(list, order)
 
			! Quick sort routine from:
			! Brainerd, W.S., Goldberg, C.H. & Adams, J.C. (1990) "Programmer's Guide to
			! Fortran 90", McGraw-Hill  ISBN 0-07-000248-7, pages 149-150.
			! Modified by Alan Miller to include an associated integer array which gives
			! the positions of the elements in the original order.
 
			!IMPLICIT NONE
			double precision, DIMENSION (:), INTENT(IN OUT)  :: list
			INTEGER, DIMENSION (:), INTENT(OUT)  :: order
		end subroutine !RECURSIVE SUBROUTINE j_quick_sort(list, order)
 
 
 
		subroutine j_startfig(iob,io,update)  !start figure drawing functions
			integer, intent(in) ::iob
			integer, intent(in) ::io
			logical,intent(in),optional::update
		end subroutine !subroutine startfig(iob,io)
 
 
 
		subroutine j_range(xmin,xmax,xmin2,xmax2)
			real, intent(in)::xmin,xmax
			real, intent(out)::xmin2,xmax2
		end subroutine
 
		subroutine j_dbw(mets,y,lkm,n,nmets,bias,sd,sb,sw,pien,suur, &
				bias2,s2out)
			integer, intent(in)::mets
			double precision, intent(in),dimension(*)::y
			integer, intent(in)::lkm
			integer*8,intent(out)::n
			integer, intent(out)::nmets
			double precision,intent(out),dimension(*) ::bias,sd,sb,sw,pien,suur,bias2
			double precision, intent(out):: s2out
		end subroutine
 
		!!!jlp !!!!!
		subroutine p_getrow(le,nterminrow,nterm,termvars,pvars,coefvars,isplus)
			integer::le !length os the left side
			integer, dimension(:), allocatable :: nterminrow,termvars,pvars,coefvars
			integer, dimension(:), allocatable ::isplus
 
		end subroutine
 
		subroutine p_getcol(icol)
			integer,intent(in)::icol
		end subroutine
 
		subroutine p_subcol(ic1,ic2,icr)   !!!!
			integer,intent(in)::ic1,ic2,icr
		end subroutine
 
		subroutine p_curix(iuni)
			integer,intent(in)::iuni
		end subroutine
 
		subroutine p_fcurix(iuni)
			integer,intent(in)::iuni
		end subroutine
 
		! subroutine p_firow2curix(iuni)
 
		! integer,intent(in)::iuni
		! end subroutine
 
 
	end interface !interface
 
 
end module jmod !!module getmod
 
!these subroutines are on the responsibility of gaya group
module gayamod
	integer j_g_npvar,j_g_ngvar,j_g_maxvar,j_g_nvar,j_g_nvarre
	integer :: j_g_maxvarold=0
	real ,dimension(:),allocatable:: j_g_xx  !pointer::j_g_xx=>null(),j_g_p=>null(),j_g_var=>null()
	real ,dimension(:),allocatable:: j_g_p
	real ,dimension(:),allocatable:: j_g_var
	integer ,dimension(:),allocatable:: j_g_ixl  !=>null()
endmodule !module gayamod
