
subroutine o1_init()
	return
end subroutine !subroutine o1_init()



subroutine o1_funcs(iob,io)
	use jmod, only: j_o
	use jomod, only: j_o1funcdelta
 
	goto(10,20,30,40,50,60,70,80,90,100,110)j_o(iob)%i(io)-j_o1funcdelta
 
		10  continue
	narg = j_o(iob)%i(io+1)
	iout = j_o(iob)%i(io+1+narg+1)
	write(6,*)'o1_function 1 done....'
	io = io + narg + 3
	return
		20 continue
		30 continue
		40 continue
		50 continue
		60 continue
		70 continue
		80 continue
		90 continue
		100 continue
		110 continue
	narg = j_o(iob)%i(io+1)
	iout = j_o(iob)%i(io+1+narg+1)
	write(6,*)'o1_function 1 done....'
	io = io + narg + 3
	return
end subroutine !subroutine o1_funcs(iob,io)

recursive subroutine o1_del(iv,iotype) !deletes subobjects of compound own-object
	integer,intent(in) :: iv  !object to be deleted
	integer,intent(in) :: iotype ! object type according to the objectype numbering of own objects
 
	!note only subobjects need to be deleted, the calling subroutine deletes integer,real
	! double precision and character forks of the object iv
	! select case(iotype)
	! case (2)  !if objecttype 2 contains subobjects
	! call del(o(iv)%i(2))  !if o(iv)%i(2) refers to subobject the subobject can be a J-type object or
	! !own-object
	! case (4)
	! !!!
	! end select
 
	return
 
end subroutine !recursive subroutine o1_del(iv,iotype)

subroutine o1_open()  !see own2 package
	use jmod, only: j_err
 
	write(6,*)'*o1_ reading not available'
	j_err=.true.
 
	return
end subroutine !subroutine o1_open()
subroutine o1_opensub()
 
 
	return
end subroutine !subroutine o1_opensub()

subroutine o1_getobs()
	return
end subroutine !subroutine o1_getobs()

subroutine o1_getsubobs()
	return
end subroutine !subroutine o1_getsubobs()
