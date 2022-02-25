 
subroutine jsleep(isec)
	USE IFPORT !nopre!
	integer(4) isec
	call sleep(isec)
	return
end subroutine
 
 
 
subroutine showdir(iob,io)
 
 
!    character*30 dirname
!   variable dirname must be long enough to hold entire string
use j_globalsmod, only: j_o
use j_globalsmod, only: j_cline
	integer(4) istat
	narg=j_o(iob)%i(io+1)

	ISTAT = GETCWD (j_cline)
	le=lentrim(j_cline)
	IF (le.gt.0) then
		write(6,*) 'Current directory is ',j_cline(1:le)
	else
		write(6,*)'**got error code ',istat,' from GETCDW '

	endif


	io=io+narg+3
	return
end
 
subroutine setdir(iob,io_)
 
use j_globalsmod, only: j_o
use j_globalsmod, only: j_otype
use j_globalsmod, only: j_IPCHAR   !!!objecttype chracter constant
use errmod, only: j_err
use getmod, only: j_getchar
use j_globalsmod, only: j_cline
	USE IFPORT  !nopre!
	LOGICAL(4) status
!  status = CHANGEDIRQQ('d:\fps90\bin))
! We are now CCed to 'd:\fps90\bin'
!  status = CHANGEDIRQQ('bessel')
! We are now CCed to 'd:\fps90\bin\bessel'
	io=io_
	narg=j_o(iob)%i(io+1)
	irg=j_o(iob)%i( io+2)
	io_=io_+narg+3
	if(j_otype(irg).ne.j_ipchar)then
		write(6,*)'**illegal setdir()'
		j_err=.true.
		return
	endif
	call j_getchar(irg,j_cline,lencline)

	status = CHANGEDIRQQ(j_cline(1:lencline))
	if(.not.status)then


		write(6,*)'**got error from CHANGEDIRQQ '
		write(6,*)'tried to set directory into ',j_cline(1:lencline)
		j_err=.true.

	end if



	return
end
