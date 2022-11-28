

module o1_mod
	use jmod, only: j_lenfunction
	use jmod, only: j_lenoption
	use jmod, only: j_lenobjecttype
	!options
	character*15 :: o1_title='own1 4.11.2015'
 
	!functions
	integer, parameter :: o1_nfunctions = 11
	character(len=j_lenfunction) :: o1_functions(o1_nfunctions)
	data o1_functions/&
		'melareport1','include1', &
		'pmin1', 'Pmax1','rpmin1','rpmax1','smin1', 'smax', 'rsmin1', 'rsmax1', &
		'koe1' &
		/
	integer, dimension(o1_nfunctions) :: o1_minarg = (/ &
		1, 1, &
		0, 2, 2, 2, 0, 0, 0, 0, &
		0 &
		/)
	integer, dimension(o1_nfunctions)::o1_maxarg = (/ &
		4, 999, &
		2, 3, 2, 2, 999, 999, 999, 999, &
		999 &
		/)
 
	integer,parameter::o1_nnamedfuncarg=1
	character(len=j_lenfunction), dimension(o1_nnamedfuncarg) :: o1_namedfuncarg
	data o1_namedfuncarg /'koe1'/
	! logical, dimension(o1_nfunctions) :: o1_namedfuncarg = (/ &
		! .false.,.false., &
		! .false.,.false.,.false.,.false.,.false.,.false.,.false.,.false., &
		! .true. &
	! /)
 
	integer, parameter :: o1_noptions = 2
	character(len=j_lenoption):: o1_options(o1_noptions)
	data	o1_options /'melaoptio11', 'melaoptio12'/
	
!options whose argumenst must be named objects:
	integer,parameter :: o1_nnamedoptarg=1
	character(len=j_lenoption), dimension(o1_nnamedoptarg):: o1_namedoptarg
	data o1_namedoptarg/'melaoptio11'/
 
! options whose named arguments can be objects which are not known beforehand:
	integer,parameter :: o1_nnewvar=1
	character(len=j_lenoption), dimension(o1_nnewvar):: o1_newvar
	data o1_newvar/'melaoptio11'/
	! logical o1_namedoptarg(o1_noptions)
	! data o1_namedoptarg/.true., .false./
	! logical :: o1_newvar(o1_noptions) =(/.true., .false./)
	
	integer,parameter :: o1_ncodeoptions=0
	character(len=j_lenoption) o1_codeoptions(1)  !otherwise compiler complains
	! data statement for code option names
 
	!object types
	integer, parameter :: o1_nobjecttypes = 2
	character(len=j_lenobjecttype), dimension (1:o1_nobjecttypes)::o1_objecttypes
	data o1_objecttypes/ &
		'FOREST_REPORT1','DATA1'&
		/
 
	! integer, parameter :: o1_nsubobjecttypes = 0
! !20141219 oli: character*16, dimension (0:j_nobjecttypes)::j_objecttypes=(/'REAL','??',&
 ! character(len=lensubobject), dimension (1:max(o1_nsubobjecttypes,1))::o1_subobjecttypes  !=(/ &
 ! data o1_subobjecttypes/ &
 ! 'TEXT2%WAD'/
 
 
		! integer, parameter ::o1_ncompoundobjects=0
	 ! !20141219 oli: character*16, dimension (0:j_nobjecttypes)::j_objecttypes=(/'REAL','??',&
		! character*(lenobject), dimension (1:max(o1_ncompoundobjects,1))::o1_compoundobjects  !=(/ &
	interface
	
	recursive subroutine o1_del(iv,iotype) !deletes subobjects of compound own-object
		integer,intent(in) :: iv  !object to be deleted
		integer,intent(in) :: iotype ! object type according to the objectype numbering of own objects
   end subroutine !recursive subroutine o1_del(iv,iotype)
 
		subroutine o1_open()  !see own2 package
		end subroutine !subroutine o1_open()
 
		subroutine o1_opensub()
		end subroutine !subroutine o1_opensub()
 
		subroutine o1_getobs()
 
		end subroutine !subroutine o1_getobs()
 
		subroutine o1_getsubobs()
		end subroutine !subroutine o1_getsubobs()
		subroutine o1_init()
	
		end subroutine !subroutine o1_init()
		subroutine o1_funcs(iob,io)
			integer,intent(in)::iob
			integer ::io
		end subroutine
	
	end interface
 
 
 
end module !module o1_mod
