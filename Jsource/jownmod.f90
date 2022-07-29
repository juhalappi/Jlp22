module jomod
	! use j_mod, only: j_ncompoundobjects,j_compoundobjects
	!	use o1_mod, only: o1_ncompoundobjects,o1_compoundobjects
	!	use o2_mod, only: o2_ncompoundobjects,o2_compoundobjects
	!	use o3_mod, only: o3_ncompoundobjects,o3_compoundobjects
 
	! parameter  (mxdo=8)
 
 
	parameter (j_nfuncs1=j_nfunctions_+o1_nfunctions)
	parameter (j_nfuncs2=j_nfuncs1+o2_nfunctions)
	parameter (j_nfunctions=j_nfuncs2+o3_nfunctions)
	integer, parameter :: j_o1funcdelta=j_nfunctions_
	integer, parameter ::  j_o2funcdelta=j_o1funcdelta+o1_nfunctions
	integer, parameter :: j_o3funcdelta=j_o2funcdelta+o2_nfunctions
	! parameter (nfunctions=j_nfunctions+o1_nfunctions+o2_nfunctions+o3_nfunctions)
	logical, dimension(j_nfunctions) :: j_isnamedfuncarg=.false.
	integer,dimension(j_nfunctions):: j_lenfunctions=0
 
 
	parameter (j_nopts1=j_noptions_+o1_noptions)
	parameter (j_nopts2=j_nopts1+o2_noptions)
	parameter (j_noptions=j_nopts2+o3_noptions)
	! parameter (noptions=j_noptions+o1_noptions+o2_noptions+o3_noptions)
	logical, dimension(j_noptions) :: j_isnamedoptarg(j_noptions)=.false.
	logical, dimension(j_noptions):: j_isnewvar=.false.
	integer,dimension(j_noptions):: j_lenoptions=0
	integer,dimension(1:j_noptions):: j_linkopt=0
	integer, dimension(j_noptions) :: j_linkopt2 !used to run options
 
	logical, dimension(j_noptions) :: j_codeoption_=.false.
 
 
	parameter (j_nobjecttypes1=j_nobjecttypes_+o1_nobjecttypes)
	parameter (j_nobjecttypes2=j_nobjecttypes1+o2_nobjecttypes)
	parameter (j_nobjecttypes3=j_nobjecttypes2+o3_nobjecttypes)
	parameter (j_nobjecttypes=j_nobjecttypes3)
	integer,dimension(j_nobjecttypes):: j_lenobjecttypes=0
 
	! parameter (ncompoundobjects1=j_ncompoundobjects+o1_ncompoundobjects)
	! parameter (ncompoundobjects2=ncompoundobjects1+o2_ncompoundobjects)
	! parameter (ncompoundobjects=ncompoundobjects2+o3_ncompoundobjects)
	! integer,dimension(nsubobjecttypes):: lensubobjecttype=0
 
	parameter (j_ncodeoptions=j_ncodeoptions_+o1_ncodeoptions+o2_ncodeoptions+o3_ncodeoptions)
	!20150202 oli: logical, dimension(noptions) :: codeoption_=.false.
 
 
	!   end module
	!module jmelamod
	! logical oinp
	! data oinp/.false./
	! integer, dimension(0:3):: ivobuf=0
	! integer,dimension(1:3):: lineobuf=0
	!	!integer :: lineobuf = 0
	! logical :: erro =.false.
	contains
 
	function j_minarg(ifunc)
		if(ifunc.gt.0.and.ifunc.le.j_nfunctions_)then
			j_minarg=j_minarg_(ifunc)
		elseif(ifunc.le.j_nfuncs1)then
			j_minarg=o1_minarg(ifunc-j_nfunctions_)
		elseif(ifunc.le.j_nfuncs2)then
			j_minarg=o2_minarg(ifunc-j_nfuncs1)
		elseif(ifunc.le.j_nfunctions)then
			j_minarg=o3_minarg(ifunc-j_nfuncs2)
		else
			write(6,*)'*j* illegal value for ifunc in minarg ',ifunc
			j_err=.true. ;return
		endif !if(ifunc.gt.0.and.ifunc.le.j_nfunctions_)     60
	end function
 
	function j_maxarg(ifunc)
		if(ifunc.gt.0.and.ifunc.le.j_nfunctions_)then
			j_maxarg=j_maxarg_(ifunc)
		elseif(ifunc.le.j_nfuncs1)then
			j_maxarg=o1_maxarg(ifunc-j_nfunctions_)
		elseif(ifunc.le.j_nfuncs2)then
			j_maxarg=o2_maxarg(ifunc-j_nfuncs1)
		elseif(ifunc.le.j_nfunctions)then
			j_maxarg=o3_maxarg(ifunc-j_nfuncs2)
		else
			write(6,*)'*j* illegal value for ifunc in maxarg ',ifunc
			j_err=.true.; return
		endif !if(ifunc.gt.0.and.ifunc.le.j_nfunctions_)     75
	end function
 
 
 
 
end module
 
