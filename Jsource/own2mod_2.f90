

module o2_mod
	use jmod, only: j_lenfunction
	use jmod, only: j_lenoption
	use jmod, only: j_lenobjecttype
	!options
	character*15 :: o2_title='own2 4.11.2014'
 
	!functions
	integer, parameter :: o2_nfunctions = 4
 
	character(len=j_lenfunction) :: o2_functions(o2_nfunctions)
	data o2_functions/&
		'example2','own22', &
		'pmin2', 'Pmax2' &
		/
	integer, dimension(o2_nfunctions) :: o2_minarg = (/ &
		0, 1, &
		0, 2&
		/)
	integer, dimension(o2_nfunctions)::o2_maxarg = (/ &
		999, 999, &
		2, 3 &
		/)
 
	integer,parameter::o2_nnamedfuncarg=1
	character(len=j_lenfunction), dimension(o2_nnamedfuncarg) :: o2_namedfuncarg
	data o2_namedfuncarg /'Pmax2'/
	! logical, dimension(o2_nfunctions) :: o2_namedfuncarg = (/ &
		! .false.,.false., &
		! .false.,.false.,.false.,.false.,.false.,.false.,.false.,.false., &
		! .true. &
	! /)
 
	integer, parameter :: o2_noptions = 2
	character(len=j_lenoption):: o2_options(o2_noptions)
	data	o2_options /'melaoptio21', 'melaoptio22'/
 
!options whose argumenst must be named objects:
	integer,parameter :: o2_nnamedoptarg=1
	character(len=j_lenoption), dimension(o2_nnamedoptarg):: o2_namedoptarg
	data o2_namedoptarg/'melaoptio21'/
 
!	options whose named arguments can be objects which are not known beforehand:
	integer,parameter :: o2_nnewvar=1
	character(len=j_lenoption), dimension(o2_nnewvar):: o2_newvar
	data o2_newvar/'melaoptio21'/
	! logical o2_namedoptarg(o2_noptions)
	! data o2_namedoptarg/.true., .false./
	! logical :: o2_newvar(o2_noptions) =(/.true., .false./)
 
 
	integer,parameter :: o2_ncodeoptions=0
	character(len=j_lenoption) o2_codeoptions(1) ! otherwise compiler warns,
!if o2_ncodeoptions>0	 use o2_codeoptions(o2_ncodeoptions)
	!data statement for code option names
 
	!object types
	integer, parameter :: o2_nobjecttypes = 2
	character(len=j_lenobjecttype), dimension (1:o2_nobjecttypes)::o2_objecttypes
	data o2_objecttypes/ &
		'FOREST_REPORT2','DATA2'&
		/
 
	! integer, parameter :: o2_nsubobjecttypes = 0
! !20141219 oli: character*16, dimension (0:j_nobjecttypes)::j_objecttypes=(/'REAL','??',&
 ! character(len=lensubobject), dimension (1:max(o2_nsubobjecttypes,1))::o2_subobjecttypes  !=(/ &
 ! data o2_subobjecttypes/ &
 ! 'TEXT2%WAD'/
 
 
		! integer, parameter ::o2_ncompoundobjects=0
	 ! !20141219 oli: character*16, dimension (0:j_nobjecttypes)::j_objecttypes=(/'REAL','??',&
		! character*(lenobject), dimension (1:max(o2_ncompoundobjects,1))::o2_compoundobjects  !=(/ &
		! data o2_compoundobjects/'first_compound'/
		interface
	
	recursive subroutine o2_del(iv,iotype) !deletes subobjects of compound own-object
		integer,intent(in) :: iv  !object to be deleted
		integer,intent(in) :: iotype ! object type according to the objectype numbering of own objects
   end subroutine !recursive subroutine o2_del(iv,iotype)
 
		subroutine o2_open()  !see own2 package
		end subroutine !subroutine o2_open()
 
		subroutine o2_opensub()
		end subroutine !subroutine o2_opensub()
 
		subroutine o2_getobs()
 
		end subroutine !subroutine o2_getobs()
 
		subroutine o2_getsubobs()
		end subroutine !subroutine o2_getsubobs()
	
		subroutine o2_init()
	
		end subroutine !subroutine o1_init()
		subroutine o2_funcs(iob,io)
			integer,intent(in)::iob
			integer ::io
		end subroutine
		subroutine o2_example(iob,io)
			integer,intent(in)::iob
			integer ::io
		end subroutine
	
		end interface
 
end module !module o2_mod
