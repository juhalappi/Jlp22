

subroutine o3_init()
	return
end subroutine !subroutine o3_init()



subroutine o3_funcs(iob,io)
	use jmod, only: j_o
	use jomod, only: j_o3funcdelta
 
	goto(10,20,30,40,50,60,70,80,90,100,110)j_o(iob)%i(io)-j_o3funcdelta
 
		10  continue
	narg = j_o(iob)%i(io+1)
	iout = j_o(iob)%i(io+1+narg+1)
	write(6,*)'o3_function1 done...'
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
	write(6,*)'o3_functionx done....'
	io = io + narg + 3
	return
	return
end subroutine !subroutine o3_funcs(iob,io)


subroutine o3_open()  !see o2 -package
	use jmod, only: j_err
 
	write(6,*)'*o3_ reading not avalaible'
	j_err=.true.
 
 
	return
end subroutine !subroutine o3_open()
subroutine o3_opensub()
 
	return
end subroutine !subroutine o3_opensub()

recursive subroutine o3_del(iv,iotype) !deletes subobjects of compound o-object
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
end !recursive subroutine o3_del(iv,iotype)

subroutine o3_getobs()
	return
end subroutine !subroutine o3_getobs()

subroutine o3_getsubobs()
	return
end subroutine !subroutine o3_getsubobs()
