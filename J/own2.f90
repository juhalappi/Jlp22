
subroutine o2_init()
	return
end subroutine !subroutine o2_init()



subroutine o2_funcs(iob,io)

!write(6,*)'<11>iob,io,j_o(iob)%i(io),o2_funcdelta,j_o(iob)%i(io)-o2_funcdelta',&
 !   iob,io,j_o(iob)%i(io),j_o(iob)%i(io)
	goto(10,20,30,40)j_o(iob)%i(io)-j_o2funcdelta

	10  call o2_example(iob,io)
	return
	20 continue !call ow2_2(iob,io);return
	30 continue !call ow2_2(iob,io);return
	40 continue !call ow2_2(iob,io);return
	narg = j_o(iob)%i(io+1)
	iout = j_o(iob)%i(io+1+narg+1)
	write(6,*)'o2_function ? done....'
	io = io + narg + 3

	return
end subroutine !subroutine o2_funcs(iob,io)

recursive subroutine o2_del(iv,iotype) !deletes subobjects of compound o-object
	integer,intent(in) :: iv  !object to be deleted
	integer,intent(in) :: iotype ! object type according to the objectype numbering of o objects

!note only subobjects need to be deleted, the calling subroutine deletes integer,real
! double precision and character forks of the object iv
! select case(iotype)
! case (2)  !if objecttype 2 contains subobjects
 ! call del(o(iv)%i(2))  !if o(iv)%i(2) refers to subobject the subobject can be a J-type object or
                         ! !o-object
 ! case (4)
 ! !!!
 ! end select

	return

end subroutine !recursive subroutine o2_del(iv,iotype)

subroutine o2_open()
!this subroutine is called to open file for reading in data function.
! as an example the same thing as if the data is read from ascii file
! the value given in form-> is stored in
! j_data_form_c  and its length in j_data_form_lenc

!	call j_openread(j_data_in_c(1:j_data_in_lenc),'*',j_data_nu)
	!this may return error code
	return
end subroutine !subroutine o2_open()
subroutine o2_opensub()

	! the value given in subform-> is stored in
! j_data_subform_c  and its length in j_data_subform_lenc
!	call j_openread(j_data_subin_c(1:j_data_subin_lenc),'*',j_data_subnu)
	return
end subroutine !subroutine o2_opensub()

subroutine o2_getobs()
!	read(j_data_nu,*,end=80,err=90)j_v(j_data_read_)
	return
!	80 j_data_eof=.true.
!	call j_closeunit(j_data_nu)
!	if(j_data_subopen)call j_closeunit(j_data_subnu)
	return
	90 write(6,*)'*o2_getobs: error in reading'
	j_err=.true.
!	call j_closeunit(j_data_nu)
!	if(j_data_subopen)call j_closeunit(j_data_subnu)
end subroutine !subroutine o2_getobs()

subroutine o2_getsubobs()
	!read(j_data_subnu,*,end=80,err=90)j_v(j_data_subread_)
	return
	80 write(6,*)'*o2_getsubobs: premature eof'

!	call j_closeunit(j_data_subnu)
	j_err=.true.
	return
	90 write(6,*)'*o2_getobs: error in reading'
	j_err=.true.
	return
end subroutine !subroutine o2_getsubobs()

subroutine o2_example(iob,io)
	integer,intent(in)::iob
	integer ::io
	integer, dimension (:), pointer :: arg

! if the function output cannot be the same as one of the arguments or option arguments
! or one of the input or output objects of transformation arguments the function should be started with
	call j_checkoutput(iob,io)
	if(j_err)return

! Then the function can be started with:
	!call  j_startfunction(iob,io,iptype,expand,narg,arg,ivout)
	call  j_startfunction(iob,io,0,.true.,narg,arg,ivout)
	write(6,*)'o2_example: there were ',narg, ' arguments'
	do i=1,narg
		write(6,*)j_object_name(arg(i),15),' type ',j_objecttypes( j_otype(arg(i)))
	enddo !do i=1,narg
	call j_printname('output was ',ivout,' ')




end subroutine !subroutine o2_example(iob,io)
