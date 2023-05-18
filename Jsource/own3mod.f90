 
 
module o3_mod
	!options
	character*15 :: o3_title='own3 4.11.2015'
 
	!functions
	integer, parameter :: o3_nfunctions = 11
	character(len=j_lenfunction) :: o3_functions(o3_nfunctions)
	data o3_functions/&
		'melareport_3','include_3', &
		'pmin_3', 'Pmax_3','rpmin_3','rpmax_3','smin_3', 'smax_3', 'rsmin_3', 'rsmax_3', &
		'koe_3' &
		/
	integer, dimension(o3_nfunctions) :: o3_minarg = (/ &
		1, 1, &
		0, 2, 2, 2, 0, 0, 0, 0, &
		0 &
		/)
	integer, dimension(o3_nfunctions)::o3_maxarg = (/ &
		4, 999, &
		2, 3, 2, 2, 999, 999, 999, 999, &
		999 &
		/)
 
	integer,parameter::o3_nnamedfuncarg=1
	character(len=j_lenfunction), dimension(o3_nnamedfuncarg) :: o3_namedfuncarg
	data o3_namedfuncarg /'koe_3'/
	! logical, dimension(o3_nfunctions) :: o3_namedfuncarg = (/ &
	! .false.,.false., &
	! .false.,.false.,.false.,.false.,.false.,.false.,.false.,.false., &
	! .true. &
	! /)
 
	integer, parameter :: o3_noptions = 2
	character(len=j_lenoption):: o3_options(o3_noptions)
	data	o3_options /'melaoptio31', 'melaoptio32'/
 
	!options whose argumenst must be named objects:
	integer,parameter :: o3_nnamedoptarg=1
	character(len=j_lenoption), dimension(o3_nnamedoptarg):: o3_namedoptarg
	data o3_namedoptarg/'melaoptio31'/
 
	!options whose named arguments can be objects which are not known beforehand:
	integer,parameter :: o3_nnewvar=1
	character(len=j_lenoption), dimension(o3_nnewvar):: o3_newvar
	data o3_newvar/'melaoptio31'/
	! logical o3_namedoptarg(o3_noptions)
	! data o3_namedoptarg/.true., .false./
	! logical :: o3_newvar(o3_noptions) =(/.true., .false./)
 
 
 
	integer,parameter :: o3_ncodeoptions=0
	character(len=j_lenoption) o3_codeoptions(1) !otherwise compiler warns,
	!if o3_ncodeoptions>0	 use o3_codeoptions(o3_ncodeoptions)
 
 
	!object types
	integer, parameter :: o3_nobjecttypes = 2
	character(len=j_lenobjecttype), dimension (1:o3_nobjecttypes)::o3_objecttypes
	data o3_objecttypes/ &
		'FOREST_REPORT3','DATA3'&
		/
 
	! integer, parameter :: o3_nsubobjecttypes = 0
	! !20141219 oli: character*16, dimension (0:j_nobjecttypes)::j_objecttypes=(/'REAL','??',&
	! character(len=lensubobject), dimension (1:max(o3_nsubobjecttypes,1))::o3_subobjecttypes  !=(/ &
	! data o3_subobjecttypes/ &
	! 'TEXT3%WAD'/
 
	! integer, parameter ::o3_ncompoundobjects=0
	! !20141219 oli: character*16, dimension (0:j_nobjecttypes)::j_objecttypes=(/'REAL','??',&
	! character*(lenobject), dimension (1:max(o3_ncompoundobjects,1))::o3_compoundobjects  !=(/ &
	! data o3_compoundobjects/'first_compound'/
	interface
 
		recursive subroutine o3_del(iv,iotype) !deletes subobjects of compound own-object
			integer,intent(in) :: iv  !object to be deleted
			integer,intent(in) :: iotype ! object type according to the objectype numbering of own objects
		end subroutine !recursive subroutine o3_del(iv,iotype)
 
		subroutine o3_open()  !see own2 package
		end subroutine !subroutine o3_open()
 
		subroutine o3_opensub()
		end subroutine !subroutine o3_opensub()
 
		subroutine o3_getobs()
 
		end subroutine !subroutine o3_getobs()
 
		subroutine o3_getsubobs()
		end subroutine !subroutine o3_getsubobs()
		subroutine o3_init()
 
		end subroutine !subroutine o1_init()
 
		subroutine o3_funcs(iob,io)
			integer,intent(in)::iob
			integer ::io
		end subroutine
	end interface
 
end module !module o3_mod
