! !this file contains gfortran system dependent subroutines

subroutine jsleep(isec)

	integer isec
	call sleep(isec)
	return
end subroutine !subroutine jsleep(isec)




subroutine showdir(iob,io)

	integer istat

	

!gfortran

	narg=j_o(iob)%i(io+1)

	ISTAT = GETCWD (j_cline)
	le=len_trim(j_cline)
	IF (le.gt.0) then
		write(6,*) 'Current directory is ',j_cline(1:le)
	else !IF (le.gt.0) then
		write(6,*)'**got error code ',istat,' from GETCDW '

	endif !IF (le.gt.0) then

!if not intel or gfortran
! write(6,*)'** dir not available in current version'
! j_err=.true.
!	io=io+narg+3
	return
end !subroutine showdir(iob,io)

subroutine setdir(iob,io_)


	integer status

	io=io_
	narg=j_o(iob)%i(io+1)
	irg=j_o(iob)%i( io+2)
	io_=io_+narg+3
	if(j_otype(irg).ne.j_ipchar)then
		write(6,*)'**illegal setdir()'
		j_err=.true.
		return
	endif !if(j_otype(irg).ne.j_ipchar)then
	call j_getchar(irg,j_cline,lencline)

! status = CHANGEDIRQQ(j_cline(1:lencline)) intel fortran
	status=chdir(j_cline(1:lencline))
	if(status.ne.0)then
		write(6,*)'**got error code ',status,' from CHDIR'
		write(6,*)'tried to set directory into ',j_cline(1:lencline)
		j_err=.true.
	endif !if(status.ne.0)then


	return
end subroutine !subroutine setdir(iob,io_)




