c    poistettu warm_start residuals refine_s
c ovat mukana tiedostossa fletcherdorig

	module lastmod  !nopre!  !search *jsp
	integer, dimension(:),allocatable::last
	integer nnzj
	end module
	
	module utilmod   

      contains

c	christen this file util.f
cut here >>>>>>>>>>>>>>>>>
* note xlen modified by j.l.
c*********************** dense matrix utilities ************************


c  Copyright, University of Dundee (R.Fletcher), June 1996
c  Current version dated 06/10/98

! search for *bug for changes made by J.L


      subroutine rsol(n,nn,nmax,R,b)  
      implicit REAL*8 (a-h,o-z)
      dimension R(*),b(*)
c  solves Rx=b where R is nxn upper triangular. Solution overwrites b.
c  R is a single suffix array: the first nmax elements contain the first row
c  of R in positions 1:n, the next nmax-1 elements contain the second row of R,
c  and so on. nn indexes the element R(n,n) (where nn=n*(3-n)/2+(n-1)*nmax)
      n1=nmax+1
      ii=nn
      b(n)=b(n)/R(nn)
      do i=n-1,1,-1
        ii=ii-n1+i
        b(i)=-scpr(-b(i),R(ii+1),b(i+1),n-i)/R(ii)
      enddo
      return
      end subroutine

      subroutine rtsol(n,nn,nmax,R,b) 
      implicit REAL*8 (a-h,o-z)
      dimension R(*),b(*)
c  solves Rt.x=b with same conventions as above
c  nn is not required on entry but is set on exit
      n2=nmax+2
      nn=1
      b(1)=b(1)/R(1)
      do i=2,n
        i1=i-1
        call mysaxpy(-b(i1),R(nn+1),b(i),n-i1)
        nn=nn+n2-i
        b(i)=b(i)/R(nn)
      enddo
      return
      end subroutine

      subroutine brots(n,nmax,k,kk,R,v)  
      implicit REAL*8 (a-h,o-z)
      dimension R(*),v(*)
      ipip=kk
      do i=k-1,1,-1
        ip=i+1
        ipi=ipip-nmax+i
        ii=ipi-1
        call angle(v(i),v(ip),cos,sin)
        call rot(n-i,R(ipi),R(ipip),cos,sin)
        v(ip)=sin*R(ii)
        R(ii)=cos*R(ii)
        ipip=ii
      enddo
      return
      end subroutine

      subroutine frots(nr,nc,nmax,R,v)
      implicit REAL*8 (a-h,o-z)
      dimension R(*),v(*)
c nr is either nc or nc+1
      ii=1
      do i=1,nc
        ip=i+1
        ipi=ii+1
        ipip=ipi+nmax-i
        call angle(R(ii),v(ip),cos,sin)
        call rot(nr-i,R(ipi),R(ipip),cos,sin)
        ii=ipip
      enddo
      return
      end subroutine

      subroutine angle(a,b,cos,sin)
      implicit REAL*8 (a-h,o-z)
      z=sqrt(a**2+b**2)
      if(z.eq.0.D0)then
        cos=1.D0
        sin=0.D0
        return
      endif
      cos=a/z
      sin=b/z
      a=z
      b=0.D0
      return
      end subroutine

      subroutine rot(n,a,b,cos,sin)
      implicit REAL*8 (a-h,o-z)
      dimension a(*),b(*)
      if(sin.eq.0.D0)then
        if(cos.gt.0.D0)then
          do i=1,n
            b(i)=-b(i)
          enddo
        else
          do i=1,n
            a(i)=-a(i)
          enddo
        endif
      elseif(cos.eq.0.D0)then
        if(sin.ge.0.D0)then
          do i=1,n
            z=a(i)
            a(i)=b(i)
            b(i)=z
          enddo
        else
          do i=1,n
            z=a(i)
            a(i)=-b(i)
            b(i)=-z
          enddo
        endif
      else
        do i=1,n
          z=a(i)
          a(i)=cos*z+sin*b(i)
          b(i)=sin*z-cos*b(i)
        enddo
      endif
      return
      end subroutine

      subroutine mysaxpy(a,x,y,n)
      implicit REAL*8 (a-h,o-z)
      dimension x(*),y(*)
      if(a.eq.0.D0)return

      do i=1,n
       y(i)=y(i)+a*x(i)
      enddo
*opt ei toimi       y(1:n)=a*x(1:n)
      return
      end subroutine

      subroutine saxpys(a,x,is,y,n)
      implicit REAL*8 (a-h,o-z)
c  saxpy with stride
      dimension x(*),y(*)
      if(a.eq.0.D0)return
      ix=1
      do i=1,n
        y(i)=y(i)+a*x(ix)
        ix=ix+is
      enddo
      return
      end subroutine

      subroutine saxpyx(a,x,y,n)
      implicit REAL*8 (a-h,o-z)
c  saxpy with result in x
      dimension x(*),y(*)
      if(a.eq.0.D0)then
        do i=1,n
          x(i)=y(i)
        enddo
*opt riksi         x(1:n)=y(1:n)
      else
        do i=1,n
          x(i)=y(i)+a*x(i)
        enddo
*opt riski       x(1:n)=y(1:n)+a*x(1:n)
      endif
      return
      end subroutine

      subroutine saxpyz(a,x,y,z,n)
      implicit REAL*8 (a-h,o-z)
c  saxpy with result in z
      dimension x(*),y(*),z(*)
      if(a.eq.0.D0)then
        do i=1,n
          z(i)=y(i)
        enddo
*opt riski       z(1:n)=y(1:n)
      else
        do i=1,n
          z(i)=y(i)+a*x(i)
        enddo
*opt riksi        z(1:n)=y(1:n)+a*x(1:n)
      endif
      return
      end subroutine

      subroutine saxpyi(a,x,y,n)
      implicit REAL*8 (a-h,o-z)
c  saxpy with interchange of x and y
      dimension x(*),y(*)
      if(a.eq.0.D0)then
        do i=1,n
          call rexch(x(i),y(i))
        enddo
      else
        do i=1,n
          z=y(i)
          y(i)=x(i)+a*y(i)
          x(i)=z
        enddo
      endif
      return
      end subroutine

      function scpr(a,x,y,n)
      implicit REAL*8 (a-h,o-z)
      dimension x(*),y(*)
      scpr=a
      do i=1,n
        scpr=scpr+x(i)*y(i)
      enddo
*opt riksi        scpr=a+dot_product(x(1:n),y(1:n))
      return
      end function

      function xlen(a,x,n)
      implicit REAL*8 (a-h,o-z)
      dimension x(*)
c  finds the l_2 length of [a:x] where a is either 0.E0 or 1.E0
c  if overflow occurs the function is calculated in a less efficient way.
c  Users who cannot trap overflow should either use this method of calculation,
c  or use the alternative routine "xlen" below which is not quite so well
c  protected against overflow.
*      external  ieee_handler, abort
      integer   ieee_flags, ieeer, ieee_handler
*      external  ieee_flags
      character out*16
      out = ''
*      ieeer = ieee_flags ( 'clearall','all','',out )
*      ieeer=ieee_handler('clear','overflow',abort)
c  this call of ieee_handler assumes that 
c         ieeer=ieee_handler('set','overflow',abort)
c  has been set in the driver. If not this call of ieee_handler and that below
c  should be removed
      xlen=a
      do i=1,n
        xlen=xlen+x(i)**2
      enddo
      xlen=sqrt(xlen)
*      ieeer=ieee_flags ( 'get','exception','',out )
      if(out.eq.'overflow')then
        call linf(n,x,xmx,i)
c       xmx=max(xmx,1.E0) %this is needed if normalization is always used
        xlen=(a/xmx)**2
        do i=1,n
          xlen=xlen+(x(i)/xmx)**2
        enddo
        xlen=xmx*sqrt(xlen)
*        ieeer=ieee_flags ( 'clear','overflow','',out )
      endif
*      ieeer=ieee_handler('set','overflow',abort)
      return
      end function
      
c     function xlen(a,x,n)
c     implicit REAL*8 (a-h,o-z)
c     dimension x(*)
c     xlen=a
c     do i=1,n
c       xlen=xlen+x(i)**2
c     enddo
c     xlen=sqrt(xlen)
c     return
c     end
      
      subroutine linf(n,x,z,iz)
      implicit REAL*8 (a-h,o-z)
      dimension x(*)
      z=0.D0
      do i=1,n
        a=abs(x(i))
        if(a.gt.z)then
          z=a
          iz=i
        endif
      enddo
      return
      end subroutine
*bug
*this is made to work only with positive k
      subroutine rshift(r,n,k)
      implicit REAL*8 (a-h,o-z)
      dimension r(*)
!      if(k.gt.0)then
        do i=1,n
          r(i)=r(i+k)
        enddo
!      elseif(k.lt.0)then
!        do i=n,1,-1
!          r(i)=r(i+k)
!        enddo
!      endif
      return
      end subroutine
*added by J.L to handle negative k
      subroutine rlshift(r,i1,n,k)
      implicit REAL*8 (a-h,o-z)
      dimension r(*)
!      if(k.gt.0)then
!        do i=1,n
!          r(i)=r(i+k)
!        enddo
!      elseif(k.lt.0)then
        do i=n+i1-1,i1,-1
          r(i)=r(i+k)
        enddo
!      endif
      return
      end subroutine
*bug
*this is made to work onlay with positive k by J.L.
      subroutine ishift(j,n,k)   !vaihda j takasin l:ksi
      implicit REAL*8 (a-h,o-z)
      dimension j(*)
*j      if(k.gt.0)then
        do i=1,n
          j(i)=j(i+k)
        enddo
*j      elseif(k.lt.0)then
*j        do i=n,1,-1
*         if(i.le.0.or.i+k.le.0)write(6,*)'n,k,',n,k
*j          l(i)=l(i+k)
*j        enddo
*j      endif
      return
      end subroutine

*bug this is added to hanle shiflt before i2

      subroutine lshift(l,i1,n,k)
      implicit REAL*8 (a-h,o-z)
      dimension l(*)
*      if(k.gt.0)then
*        do i=1,n
*          l(i)=l(i+k)
*        enddo
*      elseif(k.lt.0)then
        do i=n+i1-1,i1,-1
*         if(i.le.0.or.i+k.le.0)write(6,*)'n,k,',n,k
          l(i)=l(i+k)
        enddo
*      endif
      return
      end subroutine


      subroutine rexch(a,b)
      REAL*8 a,b,z
      z=a
      a=b
      b=z
      return
      end subroutine

      subroutine vexch(a,b,n)
      REAL*8 a,b,z
      dimension a(*),b(*)
      do i=1,n
        z=a(i)
        a(i)=b(i)
        b(i)=z
      enddo
      return
      end subroutine

      subroutine iexch(i,j)
      k=i
      i=j
      j=k
      return
      end subroutine
********end of util
      end module


!startup-> mp=-1,mq=-1    
! pivot;if leaving p =mp then  ... ,mp=p
!       if entering q=mq then ....   mq=q
! tfbsub   (nothing)
!fbsub 
! re_fact -> mp=-1  ,mq=-1
      module fletcherdmod
       contains
!*bugs    see *bug
*notify Fletcher   -see notify

      ! routines for accessing fletcher routines
!this file is from Qpdriverd.f (which conatisn also main program and cananot be linked)
      subroutine gdotx(n,x,ws,lws,v)
      implicit REAL*8 (a-h,o-z)
      dimension x(*),ws(*),lws(0:*),v(*)
      common/noutc/nout
      write(6,*)'gdotx'
      if(lws(0).eq.0)then
        a=ws(1)
        b=ws(2)
        v(1)=a*x(1)+b*x(2)
        do 1 i=2,n-1
    1   v(i)=a*x(i)+b*(x(i-1)+x(i+1))
        v(n)=b*x(n-1)+a*x(n)
      else
        do i=1,n
          v(i)=0.D0
        enddo
        ng=lws(0)
        do ij=1,ng
          i=lws(ij)
          j=lws(ng+ij)
          v(i)=v(i)+ws(ij)*x(j)
          if(i.ne.j)v(j)=v(j)+ws(ij)*x(i)
        enddo
!c       print *,'v =',(v(i),i=1,n)
      endif
      return
      end subroutine


      subroutine ratio(jmin,jmax,r,w,bl,bu,ls,amax,alpha,qqj,qqj1)
c  two-sided ratio test
      implicit REAL*8 (a-h,r-z), integer (i-q)
      common/noutc/nout
      dimension r(*),w(*),bl(*),bu(*),ls(*)
c     write(nout,*)'qqj,alpha,amax',qqj,alpha,amax
      alpha1=1.D37
      qqj1=0
      do 1 j=jmin,jmax
        i=abs(ls(j))
        wi=w(i)
        if(wi.eq.0.D0)goto1
        ri=r(i)
        if(wi.gt.0.D0)then
          if(ri.lt.0.D0)goto1
          z=ri/wi
        else
          if(ri.lt.0.D0)then
            z=ri/wi
            if(z.lt.alpha1)then
              alpha1=z
              qqj1=j
            endif
          endif
          z=(bl(i)-bu(i)+ri)/wi
        endif
        if(z.ge.amax)goto1
        amax=z
        qqj=j
    1 continue
c     write(nout,*)'qqj,qqj1,alpha1,amax',qqj,qqj1,alpha1,amax
      if(qqj1.gt.0.and.alpha1.le.amax)then
        do 2 j=jmin,jmax
          i=abs(ls(j))
          wi=w(i)
          if(wi.ge.0.D0)goto2
          ri=r(i)
          if(ri.lt.0.D0)then
            z=ri/wi
            if(z.gt.alpha1.and.z.le.amax)then
              alpha1=z
              qqj1=j
            endif
          endif
    2   continue
        alpha=alpha1
        qqj=qqj1
      else
        qqj1=0
        alpha=amax
      endif
c     write(nout,*)'alpha,amax',alpha,amax
      return
      end subroutine

      subroutine update(n,jmin,nm,a,la,g,r,w,x,bl,bu,ls,
     *  alpha,n_inf,f,tol)
      implicit REAL*8 (a-h,r-z), integer (i-q)
      dimension a(*),la(*),g(*),r(*),w(*),x(*),bl(*),bu(*),ls(*)
      n_inf=0
      f=0.D0
      do j=jmin,nm
        i=abs(ls(j))
        y=alpha*w(i)
        if(y.ne.0.D0)then
          if(i.le.n)then
            if(ls(j).gt.0)then
              x(i)=x(i)-y
            else
              x(i)=x(i)+y
            endif
          endif
          ri=r(i)-y
          s=sign(1.D0,Real(ls(j),8))
          if(y.lt.0.D0)then
            ro=bu(i)-bl(i)-ri
            if(ro.lt.ri)then
              ri=max(ro,0.D0)
              w(i)=-w(i)
              ls(j)=-ls(j)
            endif
          endif
          if(abs(ri).le.tol)ri=0.D0
          if(r(i).lt.0.D0)then
            if(ri.ge.0.D0)then
c  remove contribution to gradient
              if(i.gt.n)then
                call saipy(s,a,la,i-n,g,n)
              else
                g(i)=g(i)+s
              endif
            else
              n_inf=n_inf+1
              f=f-ri
            endif
          endif
          r(i)=ri
        elseif(r(i).lt.0.D0)then
          n_inf=n_inf+1
          f=f-r(i)
        endif
      enddo
      return
      end subroutine

      subroutine optest(jmin,jmax,r,e,ls,rp,pj)
      implicit REAL*8 (a-h,r-z), integer (i-q)
      dimension r(*),e(*),ls(*)
      rp=0.D0
      do 1 j=jmin,jmax
        i=abs(ls(j))
        ri=r(i)/e(i)
        if(ri.ge.rp)goto1
        rp=ri
        pj=j
    1 continue
      return
      end subroutine

      subroutine signs(jmin,jmax,plus,w,ls,tol)
      implicit REAL*8 (a-h,o-z)
      dimension w(*),ls(*)
      logical plus
c  change signs as necessary
      do 1 j=jmin,jmax
        i=abs(ls(j))
        wi=w(i)
        if(wi.eq.0.D0)goto1
        if(abs(wi).le.tol)then
          w(i)=0.D0
          goto1
        endif
        if(plus.neqv.ls(j).ge.0)then
          w(i)=-wi
        endif
    1 continue
      return
      end subroutine

      subroutine signst(jmin,jmax,plus,r,w,e,ls,tol)
      implicit REAL*8 (a-h,o-z)
      dimension r(*),w(*),e(*),ls(*)
      logical plus
c  transfer with sign change as necessary
      do j=jmin,jmax
        i=abs(ls(j))
        if(abs(w(i)).le.e(i)*tol)then
          r(i)=0.D0
        elseif(plus.eqv.ls(j).ge.0)then
          r(i)=w(i)
        else
          r(i)=-w(i)
        endif
      enddo
      return
      end subroutine



      subroutine check(n,nk,nm,kmax,g,a,la,x,bl,bu,r,ls,an,f,  
     *  ws,lws,ninf,peq,lp1,nk1,lev,p,alp2)
      implicit REAL*8 (a-h,r-z), integer (i-q)
      dimension g(*),a(*),la(*),x(*),bl(*),bu(*),r(*),ls(*),
     *  an(*),ws(*),lws(*)
      common/noutc/nout
      common/epsc/eps,tol,emin
      if(lev.eq.2)then
        do i=1,n
          an(i)=g(i)
        enddo
        e=alp2*sign(1.D0,Real(p,8))
        i=abs(p)
        if(i.le.n)then
          an(i)=an(i)-e
        else
         call saipy(-e,a,la,i-n,an,n)
        endif
        goto1
      endif
      j=nm*(nm+1)/2
      do i=1,nm
        j=j-abs(ls(i))
      enddo
      if(j.ne.0)write(nout,*)'indexing error'
      e=0.
      do j=nk+1,nm
        i=abs(ls(j))
        if(i.le.n)then
          s=x(i)
        else
          s=aiscpr(n,a,la,i-n,x,0.D0)
       
        endif
        if(ls(j).gt.0)then
          s=r(i)-s+bl(i)
        else
          s=r(i)+s-bu(i)
        endif
        if(abs(s).le.tol*max(1.D0,abs(r(i))))s=0.D0
        if(abs(s).gt.e)then
          e=abs(s)
          ie=i
        endif
      enddo
      if(e.gt.tol)write(nout,*)'residual error at level 1 = ',e,ie
      if(ninf.eq.0)then
        call setfg2(n,kmax,a,la,x,ff,an,ws,lws)
      else
        call setfg1(n,nm,a,la,x,bl,bu,ff,fb,an,peq,lp1,nk1,ls,r)
      endif
      gnm=sqrt(scpr(0.D0,an,an,n))
      if(lev.eq.1)then
        e=abs(ff-f)
        if(e.gt.tol*max(1.D0,abs(f)))write(nout,*)'function error = ',e,
     *    '   f(x) =',ff
      endif
    1 continue
      e=0.D0
      do j=1,nk
c       write(nout,*)'an =',(an(i),i=1,n)
        i=abs(ls(j))
        s=sign(1.D0,Real(ls(j),8))
        if(i.le.n)then
          an(i)=an(i)-s*r(i)
          if(ls(j).gt.0)then
            s=x(i)-bl(i)
          else
            s=bu(i)-x(i)
          endif
        else
       call saipy(-s*r(i),a,la,i-n,an,n)
          if(ls(j).gt.0)then
            s=aiscpr(n,a,la,i-n,x,-bl(i))
          else
            s=-aiscpr(n,a,la,i-n,x,-bu(i))
          endif
        endif
        if(abs(s).gt.e)then
          if(j.gt.peq.and.j.lt.lp1)s=0.
          e=abs(s)
          ie=i
        endif
      enddo
      if(e.gt.tol)write(nout,*)'residual error at level 2 = ',e,ie
      e=0.D0
      do i=1,n
        if(abs(an(i)).gt.e)then
          e=abs(an(i))
          ie=i
        endif
      enddo
      if(e.gt.gnm*tol)write(nout,*)'KT condition error = ',e,ie,gnm
c     if(e.gt.gnm*tol)write(nout,*)'KT cond_n errors = ',(an(i),i=1,n)
      return
      end subroutine

 
      subroutine setrp(psb,plen,alpha,xp,blp,bup,gp,rpu,lspj,infb,fb, 
     *  tol)
      implicit REAL*8 (a-h,o-z)
      logical psb,plen
      if(psb)then
        t=sign(alpha,Real(lspj,8))
        if(xp.lt.blp)then
          fb=fb+xp-blp
          xp=xp+t
          lspj=abs(lspj)
          rpu=xp-blp
          infb=infb-1
          if(fb.le.tol)fb=0.D0
          if(rpu.ge.-tol)then
            xp=blp
            gp=gp+1.D0
          endif
        elseif(xp.gt.bup)then
c         fp=fp+bup-xp
          fb=fb+bup-xp
          xp=xp+t
          lspj=-abs(lspj)
          rpu=bup-xp
          infb=infb-1
          if(fb.le.tol)fb=0.D0
          if(rpu.ge.-tol)then
            xp=bup
            gp=gp-1.D0
          endif
        else
          xp=xp+t
          rpu=bup-xp
          t=xp-blp
          if(t.le.rpu)then
            rpu=max(t,0.D0)
            lspj=abs(lspj)
          else
            rpu=max(rpu,0.D0)
            lspj=-abs(lspj)
          endif
        endif
      else
        if(plen)xp=xp+sign(alpha,Real(lspj,8))
        rpu=max(bup-blp-alpha,0.D0)
        if(alpha.le.rpu)then
          rpu=alpha
          lspj=lspj
        else
          lspj=-lspj
        endif
      endif
      if(abs(rpu).le.tol)rpu=0.D0
      return
      end subroutine

 
      subroutine reset_w(jmin,jmax,plus,n,a,la,w,ls,s,sml)
      implicit REAL*8 (a-h,o-z)
      dimension a(*),la(*),w(*),ls(*),s(*)
      logical plus
      do 1 j=jmin,jmax
        i=abs(ls(j))
        wi=w(i)
        if(wi.eq.0.D0)goto1
        if(i.le.n)then
          z=s(i)
        else
          z=aiscpr(n,a,la,i-n,s,0.D0)
        endif
        if(abs(z).le.sml)then
          w(i)=0.D0
          goto1
        endif
        if(plus.neqv.ls(j).ge.0)then
          w(i)=-z
        else
          w(i)=z
        endif
    1 continue
      return
      end subroutine

      subroutine setfg1(n,nm,a,la,x,bl,bu,f,fb,g,peq,lp1,nk1,ls,r)
      implicit REAL*8 (a-h,r-z), integer (i-q)
      dimension a(*),la(*),x(*),bl(*),bu(*),g(*),ls(*),r(*)
      do i=1,n
        g(i)=0.D0
      enddo
      fb=0.D0
      do j=peq+1,lp1-1
        i=abs(ls(j))
        if(x(i).lt.bl(i))then
          fb=fb+bl(i)-x(i)
          g(i)=g(i)-1.D0
        elseif(x(i).gt.bu(i))then
          fb=fb+x(i)-bu(i)
          g(i)=g(i)+1.D0
        endif
      enddo
      f=fb
      do j=nk1,nm
        i=abs(ls(j))
        if(r(i).lt.0.D0)then
          f=f-r(i)
          if(i.le.n)then
            g(i)=g(i)-sign(1.D0,Real(ls(j),8))
          else
            sign_=sign(1.D0,Real(ls(j),8))
          call saipy(-sign_,a,la,i-n,g,n)
          endif
        endif
      enddo
      return
      end subroutine

      subroutine setfg2(n,kmax,a,la,x,f,g,ws,lws)  
      implicit REAL*8 (a-h,o-z)
      dimension a(*),la(*),x(*),g(*),ws(*),lws(*)
      common/noutc/nout
      if(kmax.eq.0)then
        do i=1,n
          g(i)=0.D0
        enddo
      call saipy(1.D0,a,la,0,g,n)
         f=scpr(0.D0,x,g,n)
      else
        call gdotx(n,x,ws,lws,g)
       call saipy(1.D0,a,la,0,g,n)
        f=5.D-1*scpr(aiscpr(n,a,la,0,x,0.D0),g,x,n)
      endif
      return
      end subroutine

********************************************  bqpd
     

christen this file auxil.f
cut here >>>>>>>>>>>>>>>>>
c*********** auxiliary routines requiring additional storage ************

c  Copyright, University of Dundee (R.Fletcher), June 1996
c  Current version dated 18/02/99

      subroutine major_s(p,n,k,kmax,a,la,aa,ll,an,bn,rh,ak,bk,ck,lv,  
     *  ws,lws,jmin,jmax,w,ls,sgs,pp,smx,ep)
      implicit REAL*8 (a-h,r-z), integer (i-q)
      dimension a(*),la(*),aa(*),ll(*),an(*),bn(*),rh(*),
     *  ak(*),bk(*),ck(*),lv(*),ws(*),lws(*),w(*),ls(*)
      common/noutc/nout
      common/iprintc/iprint
c     write(nout,*)'major_s  p =',p
      call tfbsub(n,a,la,p,an,an,aa,ll,ep,.true.)
c  compute  s = Y.e_p - Z.M^(-1).Zt.G.Y.e_p  in an
      do i=1,n
        w(i)=an(i)
      enddo
c     write(nout,*)'s =',(w(i),i=1,n)
      if(kmax.gt.0)then
        call linf(n,an,smx,pp)
        call gdotx(n,an,ws,lws,bn)
c       write(nout,*)'G.Y.e_p =',(bn(i),i=1,n)
        sgs=scpr(0.D0,an,bn,n)
        if(k.gt.0)then
c         write(nout,*)'Y.e_p =',(an(i),i=1,n)
          call ztaq(0,n,k,a,la,bn,bk,w,lv,aa,ll)
c  compute  R^(-T).Zt.G.Y.e_p  in ak (ak and bk also used in extend)
c         write(nout,*)'Z#t.G.Y.e_p =',(bk(i),i=1,k),sgs
          do i=1,k
            ak(i)=bk(i)
          enddo
          call rtsol(k,kk,kmax,rh,ak)
c         write(nout,*)'R^(-T).Zt.G.Y.e_p =',(ak(i),i=1,k+1)
          do i=1,k
            ck(i)=ak(i)
          enddo
          call rsol(k,kk,kmax,rh,ck)
c         write(nout,*)'ck =',(ck(i),i=1,k)
          call zprod(p,n,k,a,la,w,ck,lv,w,ls,aa,ll)
          bk(k+1)=sgs
          sgs=-scpr(-sgs,ak,ak,k)
        endif
c       write(nout,*)'sgs =',sgs
      else
        sgs=0.D0
      endif
c     write(nout,*)'s =',(w(i),i=1,n)
c  form At.s
      do j=jmin,jmax
        i=abs(ls(j))
        if(i.gt.n)then
          w(i)=aiscpr(n,a,la,i-n,w,0.D0)
        endif
      enddo
      return
      end subroutine

      subroutine minor_s(n,k,kmax,a,la,aa,ll,an,rh,ak,rg,rs,lv,  
     *  jmin,jmax,w,ls,sg,sgs)
      implicit REAL*8 (a-h,r-z), integer (i-q)
      dimension a(*),la(*),aa(*),ll(*),an(*),rh(*),ak(*),rg(*),rs(*),
     *  lv(*),w(*),ls(*)
      common/noutc/nout
      common/minorc/c
c     write(nout,*)'minor_s'
      if(sgs.gt.0.D0)then
        do i=1,k
          rs(i)=rg(i)
        enddo
        call rtsol(k,kk,kmax,rh,rs)
        sgs=scpr(0.D0,rs,rs,k)
        sg=sgs
        if(sgs.eq.0.D0)return
        call rsol(k,kk,kmax,rh,rs)
      else
        do i=1,k
          rs(i)=ak(i)
        enddo
        call rtsol(k,kk,kmax,rh,rs)
        sgs=scpr(0.D0,rs,rs,k)
        c=min(1.D0-sgs,0.D0)
        sgs=sgs*c
        call rsol(k,kk,kmax,rh,rs)
        sg=scpr(0.D0,rs,rg,k)
        if(sg.lt.0.D0)then
          do i=1,k
            rs(i)=-rs(i)
          enddo
          sg=-sg
          c=-c
        endif
      endif
c     write(nout,*)'rs =',(rs(i),i=1,k)
c     write(nout,*)'sg,sgs,c',sg,sgs,c
      call zprod(0,n,k,a,la,an,rs,lv,w,ls,aa,ll)
c     write(nout,*)'s =',(an(i),i=1,n)
c  form At.s
      do j=jmin,jmax
        i=abs(ls(j))
        if(i.gt.n)then
          w(i)=aiscpr(n,a,la,i-n,an,0.D0)
        else
          w(i)=an(i)
        endif
      enddo
      return
      end subroutine

      subroutine extend(k,kmax,p,rh,ak,bk,lv,sgs)  
      implicit REAL*8 (a-h,r-z), integer (i-q)
      dimension rh(*),ak(*),bk(*),lv(*)
      common/noutc/nout
      common/epsc/eps,tol,emin
c  extend factors of M
      kp=k+1
      lv(kp)=p
      ij=kp
      if(sgs.gt.0.D0)then
c       write(nout,*)'ak =',(ak(i),i=1,k),'   sgs =',sgs
        do i=1,k
          rh(ij)=ak(i)
          ij=ij+kmax-i
        enddo
        rh(ij)=sqrt(sgs)
      else
        u=bk(kp)
c       if(u.gt.1.E0)then
c         b=sqrt(u)
c         c=1.E0/b
c       else
c         b=1.E0
c         c=1.E0/(1.E0+sqrt(1.E0-u))
c       endif
c       a=(c*u-1.E0/c)*5.E-1
        bsq=max(tol,scpr(0.D0,bk,bk,k)/rh(1)**2+abs(u))
        b=sqrt(bsq)
        c=1.D0/(b+sqrt(bsq-u))
        a=c*u-b
        do i=1,k
          bk(i)=c*bk(i)
          ak(i)=bk(i)
          rh(ij)=0.D0
          ij=ij+kmax-i
        enddo
        ak(kp)=a
        bk(kp)=b
        ij=1
        do i=1,k
          call angle(rh(ij),bk(i),cos,sin)
          ij=ij+1
          call rot(kp-i,rh(ij),bk(i+1),cos,sin)
          ij=ij+kmax-i
        enddo
        rh(ij)=bk(kp)
      endif
      k=kp
c     write(nout,*)'extend: V-list =',(lv(i),i=1,k)
c     write(nout,*)'extended reduced Hessian factor'
c     ij=0
c     do i=1,k
c       write(nout,*)(rh(ij+j),j=1,k-i+1)
c       ij=ij+kmax-i+1
c     enddo
c     if(sgs.le.0.E0)write(nout,*)'negative part:  a =',(ak(i),i=1,k)
      return
      end subroutine

      subroutine rgup(k,ak,rg,alpha,sgs)  
      implicit REAL*8 (a-h,o-z)
      dimension ak(*),rg(*)
      common/noutc/nout
      common/minorc/c
      if(sgs.gt.0.D0)then
        c=1.D0-alpha
        do i=1,k
          rg(i)=rg(i)*c
        enddo
      else
        call mysaxpy(-alpha*c,ak,rg,k)
      endif
c     write(nout,*)'reduced gradient =',(rg(i),i=1,k)
      return 
      end subroutine

      subroutine zprod(p,n,k,a,la,an,ak,lv,w,ls,aa,ll)  
      implicit REAL*8 (a-h,r-z), integer (i-q)
      dimension a(*),la(*),an(*),ak(*),lv(*),w(*),ls(*),aa(*),ll(*)
      common/noutc/nout
      do i=1,n-k
        w(abs(ls(i)))=0.D0
      enddo
      if(p.gt.0)w(p)=1.D0
      do i=1,k
        w(lv(i))=-ak(i)
      enddo
      call tfbsub(n,a,la,0,w,an,aa,ll,ep,.false.)
      return
      end subroutine

      subroutine qinv(q,qv,k,ak,lv)  
      implicit REAL*8 (a-h,r-z), integer (i-q)
      dimension ak(*),lv(*)
c  look for q in V-list
      do i=1,k
        if(q.eq.lv(i))then
          do j=1,k
            ak(j)=0.D0
          enddo
          qv=i
          ak(qv)=1.D0
          return
        endif
      enddo
      qv=0
      return
      end subroutine

      subroutine ztaq(q,n,k,a,la,an,ak,w,lv,aa,ll)  
      implicit REAL*8 (a-h,r-z), integer (i-q)
      dimension a(*),la(*),an(*),ak(*),w(*),lv(*),aa(*),ll(*)
      common/noutc/nout
      call fbsub(n,1,k,a,la,q,an,w,lv,aa,ll,.false.)
      do i=1,k
        ak(i)=w(lv(i))
      enddo
c     write(nout,*)'Zt.aq =',(ak(i),i=1,k)
      return
      end subroutine

      subroutine reduce(k,kmax,p,pv,rh,rg,lv,ak,bk,ck,sgs,rg_up)  
      implicit REAL*8 (a-h,r-z), integer (i-q)
      dimension rh(*),rg(*),lv(*),ak(*),bk(*),ck(*)
      common/noutc/nout
      logical rg_up
c     write(nout,*)'reduce:  p =',p,'   pv =',pv
c  remove column pv from V and update reduced Hessian factors
c     write(nout,*)'ak =',(ak(i),i=1,k)
c     write(nout,*)'bk =',(bk(i),i=1,k)
      ij=pv
      do i=1,pv
        ck(i)=rh(ij)
        call rshift(rh(ij),k-pv,1)
        ij=ij+kmax-i
      enddo
      kk=ij-kmax+pv
      kmx1=kmax+1
      ij=ij+1
      za=ak(pv)
      zb=bk(pv)
      zr=rg(pv)
      do i=pv+1,k
        ck(i)=rh(ij)
        call rshift(rh(ij),k-i,1)
        im=i-1
        ak(im)=ak(i)
        bk(im)=bk(i)
        rg(im)=rg(i)
        lv(im)=lv(i)
        ij=ij+kmx1-i
      enddo
c     write(nout,*)'ck =',(ck(i),i=1,k)
      k=k-1
      if(k.eq.0)return
c     write(nout,*)'V list =',(lv(i),i=1,k)
c  return to triangular form
      if(p.ne.0)then
        call brots(k,kmax,pv,kk,rh,ck)
        call mysaxpy(-ck(1)/zb,bk,rh,k)
        call frots(k,k,kmax,rh,ck)
        if(sgs.le.0.D0)call mysaxpy(-za/zb,bk,ak,k)
      else
        call frots(k-pv+1,k-pv+1,kmax-pv+1,rh(kk),ck(pv))
      endif
      if(rg_up)call mysaxpy(-zr/zb,bk,rg,k)
      if(sgs.le.0.D0)then
c  check if negative part can be removed
        do i=1,k
          bk(i)=ak(i)
        enddo
        call rtsol(k,kk,kmax,rh,bk)
        sgs=-scpr(-1.D0,bk,bk,k)
        if(sgs.gt.0.D0)then
          call brots(k,kmax,k,kk,rh,bk)
          sgs=sqrt(sgs)
          do i=1,k
            rh(i)=rh(i)*sgs
          enddo
          call frots(k,k-1,kmax,rh,bk)
        endif
      endif
c     write(nout,*)'reduced Hessian factor'
c     ij=0
c     do i=1,k
c       write(nout,*)(rh(ij+j),j=1,k-i+1)
c       ij=ij+kmx1-i
c     enddo
c     if(sgs.le.0.E0)write(nout,*)'negative part:  a =',(ak(i),i=1,k)
c     write(nout,*)'reduced gradient',(rg(i),i=1,k)
      return
      end subroutine

      subroutine revise(k,kmax,rh,rg,lv,q,ak,bk,ck,sgs)  
      implicit REAL*8 (a-h,r-z), integer (i-q)
      dimension rh(*),rg(*),lv(*),ak(*),bk(*),ck(*)
      common/noutc/nout
c     write(nout,*)'revise'
c     write(nout,*)'bk =',(bk(i),i=1,k)
      smx=bk(k)
      lv(k)=q
      km=k-1
      ij=k
      do i=1,k
        ck(i)=rh(ij)/smx
        rh(ij)=0.D0
        ij=ij+kmax-i
      enddo
      ij=ij-kmax+k
c     write(nout,*)'ak =',(ak(i),i=1,k)
c     write(nout,*)'ck =',(ck(i),i=1,k)
      call brots(km,kmax,k,ij,rh,ck)
      call mysaxpy(-ck(1),bk,rh,km)
      rh(k)=ck(1)
      call frots(k,km,kmax,rh,ck)
      if(sgs.le.0.D0)then
        ak(k)=ak(k)/smx
        call mysaxpy(-ak(k),bk,ak,km)
      endif
      rg(k)=rg(k)/smx
      call mysaxpy(-rg(k),bk,rg,km)
c     write(nout,*)'V list =',(lv(i),i=1,k)
c     write(nout,*)'revised reduced Hessian factor'
c     ij=0
c     do i=1,k
c       write(nout,*)(rh(ij+j),j=1,k-i+1)
c       ij=ij+kmax-i+1
c     enddo
c     if(sgs.le.0.E0)write(nout,*)'negative part:  a =',(ak(i),i=1,k)
c     write(nout,*)'reduced gradient',(rg(i),i=1,k)
      return
      end subroutine

      subroutine hot_start(n,k,kmax,a,la,x,an,bn,rh,ak,lv,w,ls,aa,ll,  
     *  ws,lws,hstep)
      implicit REAL*8 (a-h,o-z)
      dimension a(*),la(*),x(*),an(*),bn(*),rh(*),ak(*),lv(*),
     *  w(*),ls(*),aa(*),ll(*),ws(*),lws(*)
      common/noutc/nout
      common/iprintc/iprint
c     dimension Z(100,20)
c     write(nout,*)'Zt matrix'
c     do i=1,k
c       call tfbsub(n,a,la,lv(i),w,Z(1,i),aa,ll,ep,.false.)
c       write(nout,*)lv(i),':',(Z(j,i),j=1,n)
c     enddo
      call gdotx(n,x,ws,lws,an)
      call saipy(1.D0,a,la,0,an,n)
c     write(nout,*)'g =',(an(i),i=1,n)
      call ztaq(0,n,k,a,la,an,ak,w,lv,aa,ll)
c     write(nout,*)'Zt.g =',(ak(i),i=1,k)
      call rtsol(k,kk,kmax,rh,ak)
      call rsol(k,kk,kmax,rh,ak)
c     write(nout,*)'M^(-1).Zt.g =',(ak(i),i=1,k)
      call zprod(0,n,k,a,la,an,ak,lv,w,ls,aa,ll)
c     write(nout,*)'Z.M^(-1).Zt.g =',(an(i),i=1,n)
      do j=1,n-k
        i=abs(ls(j))
        if(i.le.n)an(i)=0.D0
      enddo
      hstep=sqrt(scpr(0.D0,an,an,n))
      if(iprint.ge.1)write(nout,*)'norm of horizontal step =',hstep
      do i=1,n
        bn(i)=x(i)
        x(i)=x(i)+an(i)
      enddo
c     write(nout,*)'x =',(x(i),i=1,n)
      return
      end subroutine

      subroutine checkrh(n,a,la,k,kmax,rh,rg,ak,lv,aa,ll,x,ws,lws,  
     *  sgs,tol)
      implicit REAL*8 (a-h,r-z), integer (i-q)
      dimension a(*),la(*),rh(*),rg(*),ak(*),lv(*),aa(*),ll(*),
     *  x(*),ws(*),lws(*)
      dimension Z(500,20),E(20,20),v(20),w(500)
      common/noutc/nout
      if(n.gt.500.or.k.gt.20)then
        write(6,*)'extend local storage for checkrh'
        stop
      endif
c     write(nout,*)'checkrh'
c     write(nout,*)'Zt matrix'
      do i=1,k
        call tfbsub(n,a,la,lv(i),w,Z(1,i),aa,ll,ep,.false.)
c       write(nout,*)lv(i),':',(Z(j,i),j=1,n)
      enddo
      call gdotx(n,x,ws,lws,w)
      call saipy(1.D0,a,la,0,w,n)
c     write(nout,*)'g =',(w(i),i=1,n)
      do i=1,k
        v(i)=scpr(0.D0,w,Z(1,i),n)
      enddo
c     write(nout,*)'reduced gradient vector Zt.g',(v(i),i=1,k)
      emax=0.D0
      do i=1,k
        emax=max(emax,abs(rg(i)-v(i)))
      enddo
      if(emax.gt.tol)
     *  write(nout,*)'max error in reduced gradient =',emax
c     write(nout,*)'Zt.G.Z matrix'
      do i=1,k
        call gdotx(n,Z(1,i),ws,lws,w)
        do j=1,k
          E(i,j)=scpr(0.D0,w,Z(1,j),n)
        enddo
c       write(nout,*)(E(i,j),j=1,k)
      enddo
      ij=1
      do i=1,k
        do j=i,k
          w(j)=rh(ij+j-i)
        enddo
        do j=i,k
          do jj=i,k
            E(j,jj)=E(j,jj)-w(j)*w(jj)
          enddo
        enddo
        ij=ij+kmax-i+1
      enddo
      if(sgs.lt.0.D0)then
        do j=1,k
          do jj=1,k
            E(j,jj)=E(j,jj)+ak(j)*ak(jj)
          enddo
        enddo
      endif
c     write(nout,*)'error  Zt.G.Z - Rt.R'
      emax=0.D0
      do i=1,k
        do j=1,k
          emax=max(emax,abs(E(i,j)))
        enddo
c       write(nout,*)(E(i,j),j=1,k)
      enddo
      if(emax.gt.tol)write(nout,*)'max error in Rt.R =',emax
      return
      end subroutine

*************** end of auxil
christen this file util.f
cut here >>>>>>>>>>>>>>>>>
* note xlen modified by j.l.
c*********************** dense matrix utilities ************************


c  Copyright, University of Dundee (R.Fletcher), June 1996
c  Current version dated 06/10/98

! search for *bug for changes made by J.L


      subroutine rsol(n,nn,nmax,R,b)  
      implicit REAL*8 (a-h,o-z)
      dimension R(*),b(*)
c  solves Rx=b where R is nxn upper triangular. Solution overwrites b.
c  R is a single suffix array: the first nmax elements contain the first row
c  of R in positions 1:n, the next nmax-1 elements contain the second row of R,
c  and so on. nn indexes the element R(n,n) (where nn=n*(3-n)/2+(n-1)*nmax)
      n1=nmax+1
      ii=nn
      b(n)=b(n)/R(nn)
      do i=n-1,1,-1
        ii=ii-n1+i
        b(i)=-scpr(-b(i),R(ii+1),b(i+1),n-i)/R(ii)
      enddo
      return
      end subroutine

      subroutine rtsol(n,nn,nmax,R,b)  
      implicit REAL*8 (a-h,o-z)
      dimension R(*),b(*)
c  solves Rt.x=b with same conventions as above
c  nn is not required on entry but is set on exit
      n2=nmax+2
      nn=1
      b(1)=b(1)/R(1)
      do i=2,n
        i1=i-1
        call mysaxpy(-b(i1),R(nn+1),b(i),n-i1)
        nn=nn+n2-i
        b(i)=b(i)/R(nn)
      enddo
      return
      end subroutine

      subroutine brots(n,nmax,k,kk,R,v)  
      implicit REAL*8 (a-h,o-z)
      dimension R(*),v(*)
      ipip=kk
      do i=k-1,1,-1
        ip=i+1
        ipi=ipip-nmax+i
        ii=ipi-1
        call angle(v(i),v(ip),cos,sin)
        call rot(n-i,R(ipi),R(ipip),cos,sin)
        v(ip)=sin*R(ii)
        R(ii)=cos*R(ii)
        ipip=ii
      enddo
      return
      end subroutine

      subroutine frots(nr,nc,nmax,R,v)  
      implicit REAL*8 (a-h,o-z)
      dimension R(*),v(*)
c nr is either nc or nc+1
      ii=1
      do i=1,nc
        ip=i+1
        ipi=ii+1
        ipip=ipi+nmax-i
        call angle(R(ii),v(ip),cos,sin)
        call rot(nr-i,R(ipi),R(ipip),cos,sin)
        ii=ipip
      enddo
      return
      end subroutine

      subroutine angle(a,b,cos,sin)  
      implicit REAL*8 (a-h,o-z)
      z=sqrt(a**2+b**2)
      if(z.eq.0.D0)then
        cos=1.D0
        sin=0.D0
        return
      endif
      cos=a/z
      sin=b/z
      a=z
      b=0.D0
      return
      end subroutine

      subroutine rot(n,a,b,cos,sin)  
      implicit REAL*8 (a-h,o-z)
      dimension a(*),b(*)
      if(sin.eq.0.D0)then
        if(cos.gt.0.D0)then
          do i=1,n
            b(i)=-b(i)
          enddo
        else
          do i=1,n
            a(i)=-a(i)
          enddo
        endif
      elseif(cos.eq.0.D0)then
        if(sin.ge.0.D0)then
          do i=1,n
            z=a(i)
            a(i)=b(i)
            b(i)=z
          enddo
        else
          do i=1,n
            z=a(i)
            a(i)=-b(i)
            b(i)=-z
          enddo
        endif
      else
        do i=1,n
          z=a(i)
          a(i)=cos*z+sin*b(i)
          b(i)=sin*z-cos*b(i)
        enddo
      endif
      return
      end subroutine

      subroutine mysaxpy(a,x,y,n)  
      implicit REAL*8 (a-h,o-z)
      dimension x(*),y(*)
      if(a.eq.0.D0)return

      do i=1,n
       y(i)=y(i)+a*x(i)
      enddo
*opt ei toimi       y(1:n)=a*x(1:n)
      return
      end subroutine

      subroutine saxpys(a,x,is,y,n)  
      implicit REAL*8 (a-h,o-z)
c  saxpy with stride
      dimension x(*),y(*)
      if(a.eq.0.D0)return
      ix=1
      do i=1,n
        y(i)=y(i)+a*x(ix)
        ix=ix+is
      enddo
      return
      end subroutine

      subroutine saxpyx(a,x,y,n)  
      implicit REAL*8 (a-h,o-z)
c  saxpy with result in x
      dimension x(*),y(*)
      if(a.eq.0.D0)then
        do i=1,n
          x(i)=y(i)
        enddo
*opt riksi         x(1:n)=y(1:n)
      else
        do i=1,n
          x(i)=y(i)+a*x(i)
        enddo
*opt riski       x(1:n)=y(1:n)+a*x(1:n)
      endif
      return
      end subroutine

      subroutine saxpyz(a,x,y,z,n)  
      implicit REAL*8 (a-h,o-z)
c  saxpy with result in z
      dimension x(*),y(*),z(*)
      if(a.eq.0.D0)then
        do i=1,n
          z(i)=y(i)
        enddo
*opt riski       z(1:n)=y(1:n)
      else
        do i=1,n
          z(i)=y(i)+a*x(i)
        enddo
*opt riksi        z(1:n)=y(1:n)+a*x(1:n)
      endif
      return
      end subroutine

      subroutine saxpyi(a,x,y,n)  
      implicit REAL*8 (a-h,o-z)
c  saxpy with interchange of x and y
      dimension x(*),y(*)
      if(a.eq.0.D0)then
        do i=1,n
          call rexch(x(i),y(i))
        enddo
      else
        do i=1,n
          z=y(i)
          y(i)=x(i)+a*y(i)
          x(i)=z
        enddo
      endif
      return
      end subroutine

      function scpr(a,x,y,n)  
      implicit REAL*8 (a-h,o-z)
      dimension x(*),y(*)
      scpr=a
      do i=1,n
        scpr=scpr+x(i)*y(i)
      enddo
*opt riksi        scpr=a+dot_product(x(1:n),y(1:n))
      return
      end function

      function xlen(a,x,n)  
      implicit REAL*8 (a-h,o-z)
      dimension x(*)
c  finds the l_2 length of [a:x] where a is either 0.E0 or 1.E0
c  if overflow occurs the function is calculated in a less efficient way.
c  Users who cannot trap overflow should either use this method of calculation,
c  or use the alternative routine "xlen" below which is not quite so well
c  protected against overflow.
*      external  ieee_handler, abort
      integer   ieee_flags, ieeer, ieee_handler
*      external  ieee_flags
      character out*16
      out = ''
*      ieeer = ieee_flags ( 'clearall','all','',out )
*      ieeer=ieee_handler('clear','overflow',abort)
c  this call of ieee_handler assumes that 
c         ieeer=ieee_handler('set','overflow',abort)
c  has been set in the driver. If not this call of ieee_handler and that below
c  should be removed
      xlen=a
      do i=1,n
        xlen=xlen+x(i)**2
      enddo
      xlen=sqrt(xlen)
*      ieeer=ieee_flags ( 'get','exception','',out )
      if(out.eq.'overflow')then
        call linf(n,x,xmx,i)
c       xmx=max(xmx,1.E0) %this is needed if normalization is always used
        xlen=(a/xmx)**2
        do i=1,n
          xlen=xlen+(x(i)/xmx)**2
        enddo
        xlen=xmx*sqrt(xlen)
*        ieeer=ieee_flags ( 'clear','overflow','',out )
      endif
*      ieeer=ieee_handler('set','overflow',abort)
      return
      end function
      
c     function xlen(a,x,n)
c     implicit REAL (a-h,o-z)
c     dimension x(*)
c     xlen=a
c     do i=1,n
c       xlen=xlen+x(i)**2
c     enddo
c     xlen=sqrt(xlen)
c     return
c     end
      
      subroutine linf(n,x,z,iz)  
      implicit REAL*8 (a-h,o-z)
      dimension x(*)
      z=0.D0
      do i=1,n
        a=abs(x(i))
        if(a.gt.z)then
          z=a
          iz=i
        endif
      enddo
      return
      end subroutine
*bug
*this is made to work only with positive k
      subroutine rshift(r,n,k)  
      implicit REAL*8 (a-h,o-z)
      dimension r(*)
!      if(k.gt.0)then
        do i=1,n
          r(i)=r(i+k)
        enddo
!      elseif(k.lt.0)then
!        do i=n,1,-1
!          r(i)=r(i+k)
!        enddo
!      endif
      return
      end subroutine
*added by J.L to handle negative k
      subroutine rlshift(r,i1,n,k)  
      implicit REAL*8 (a-h,o-z)
      dimension r(*)
!      if(k.gt.0)then
!        do i=1,n
!          r(i)=r(i+k)
!        enddo
!      elseif(k.lt.0)then
        do i=n+i1-1,i1,-1
          r(i)=r(i+k)
        enddo
!      endif
      return
      end subroutine
*bug
*this is made to work onlay with positive k by J.L.
      subroutine ishift(l,n,k)  
      implicit REAL*8 (a-h,o-z)
      dimension l(*)
*j      if(k.gt.0)then
        do i=1,n
          l(i)=l(i+k)
        enddo
*j      elseif(k.lt.0)then
*j        do i=n,1,-1
*         if(i.le.0.or.i+k.le.0)write(6,*)'n,k,',n,k
*j          l(i)=l(i+k)
*j        enddo
*j      endif
      return
      end subroutine

*bug this is added to hanle shiflt before i2

      subroutine lshift(l,i1,n,k)  
      implicit REAL*8 (a-h,o-z)
      dimension l(*)
*      if(k.gt.0)then
*        do i=1,n
*          l(i)=l(i+k)
*        enddo
*      elseif(k.lt.0)then
        do i=n+i1-1,i1,-1
*         if(i.le.0.or.i+k.le.0)write(6,*)'n,k,',n,k
          l(i)=l(i+k)
        enddo
*      endif
      return
      end subroutine


      subroutine rexch(a,b)  
      REAL*8 a,b,z
      z=a
      a=b
      b=z
      return
      end subroutine

      subroutine vexch(a,b,n)  
      REAL*8 a,b,z
      dimension a(*),b(*)
      do i=1,n
        z=a(i)
        a(i)=b(i)
        b(i)=z
      enddo
      return
      end subroutine

      subroutine iexch(i,j)  
      k=i
      i=j
      j=k
      return
      end subroutine
********end of util
christen this file    denseA.f
cut here >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

c  ******************************************
c  Specification of A in dense matrix format
c  ******************************************

c  The matrix A contains gradients of the linear terms in the objective
c  function (column 0) and the general constraints (columns 1:m).
c  No explicit reference to simple bound constraints is required in A.
c  The information is set in the parameters a and la of bqpd.

c  In this case A is set in standard matrix format as a(la,0:m), where la
c  is the stride between columns. la is an integer that must be greater or
c  equal to n.

c  In the straightforward case that la=n, columns of A follow successively
c  in the space occupied by a(.).

c  Copyright, University of Dundee (R.Fletcher), June 1996
c  Current version dated 21/05/98

      subroutine saipy(s,a,la,i,y,n)  
      implicit REAL*8 (a-h,r-z), integer (i-q)
*bug      dimension a(la,0:*),y(*)
       dimension a(*),la(*),y(*)
*this is called so that la is a vector and a is also a vector
c  saxpy with column i of A
*bug      call mysaxpy(s,a(1,i),y,n)
       call mysaxpy(s,a(i*la(1)+1),y,n)
      return
      end subroutine

      subroutine isaipy(s,a,la,i,y,n,lr,li)  
      implicit REAL*8 (a-h,r-z), integer (i-q)
*bug      dimension a(la,0:*),y(*),lr(*),li(*)
      dimension a(*),y(*),lr(*),li(*),la(*)
c  indirectly addressed saxpy with column i of A
*bug      call isaxpy(s,a(1,i),lr,y,n)
      call isaxpy(s,a(i*la(1)+1),lr,y,n)
      return
      end subroutine

      subroutine isaipy1(s,a,la,i,y,n,lr,li,m1)  
      implicit REAL*8 (a-h,r-z), integer (i-q)
*bug      dimension a(la,0:*),y(*),lr(*),li(*)
      dimension a(*),y(*),lr(*),li(*),la(*)
c  indirectly addressed saxpy with column i of A_1
*bug      call isaxpy(s,a(1,i),lr,y,m1)
      call isaxpy(s,a(i*la(1)+1),lr,y,m1)
      return
      end subroutine

c     subroutine isaipy2(s,a,la,i,y,n,lr,li,m1)
c     implicit REAL (a-h,r-z), integer (i-q)
c     dimension a(la,0:*),y(*),lr(*),li(*)
c  indirectly addressed saxpy with column i of A_2
c     call isaxpy(s,a(1,i),lr(m1+1),y(m1+1),n-m1)
c     return
c     end

c     subroutine ssaipy(s,a,la,i,y,n)
c     implicit REAL (a-h,r-z), integer (i-q)
c     dimension a(la,0:*),y(*)
c  ssaxpy with column i of A
c     call ssaxpy(s,a(1,i),y,n)
c     return
c     end

c     subroutine ssaxpy(a,x,y,n)
c     implicit REAL (a-h,r-z), integer (i-q)
c     dimension x(*),y(*)
c  saxpy with squares of x
c     do i=1,n
c       y(i)=y(i)+a*x(i)**2
c     enddo
c     return
c     end

      function aiscpr(n,a,la,i,x,b)  
      implicit REAL*8 (a-h,r-z), integer (i-q)
*bug      dimension a(la,0:*),x(*)
        dimension a(*),x(*),la(*)
c  scalar product with column i of A
*bug      aiscpr=scpr(b,a(1,i),x,n)
       aiscpr=scpr(b,a(i*la(1)+1),x,n)
      return
      end function
* 6.4. 2009: tämä funktio määritelty kahteen kertaan tämä määrittely kommentoitu
c      function daiscpr(n,a,la,i,x,b)
c      implicit REAL*8 (a-h,r-z), integer (i-q)
*bug      dimension a(la,0:*),x(*)
c      dimension a(*),x(*),la(*)
c     DOUBLE PRECISION daiscpr,dscpr
*bug      daiscpr=dscpr(b,a(1,i),x,n)
c      daiscpr=dscpr(b,a(i*la(1)+1),x,n)
c     return
c      end function

      function aiscpri(n,a,la,i,x,b,lr,li)  
      implicit REAL*8 (a-h,r-z), integer (i-q)
*bug      dimension a(la,0:*),x(*),lr(*),li(*)
      dimension a(*),x(*),lr(*),li(*),la(*)
c  indirectly addressed scalar product with column i of A
*bug      aiscpri=scpri(b,a(1,i),lr,x,n)
      aiscpri=scpri(b,a(i*la(1)+1),lr,x,n)
      return
      end function

      function daiscpri(n,a,la,i,x,b,lr,li)  
      implicit REAL*8 (a-h,r-z), integer (i-q)
      dimension a(la,0:*),x(*),lr(*),li(*)
      DOUBLE PRECISION daiscpri
      daiscpri=dscpri(b,a(1,i),lr,x,n)
      return
      end function

      function aiscpri1(n,a,la,i,x,b,lr,li,m1)  
      implicit REAL*8 (a-h,r-z), integer (i-q)
*bug      dimension a(la,0:*),x(*),lr(*),li(*)
      dimension a(*),x(*),lr(*),li(*),la(*)
c  indirectly addressed scalar product with column i of A_1
*bug      aiscpri1=scpri(b,a(1,i),lr,x,m1)
      aiscpri1=scpri(b,a(i*la(1)+1),lr,x,m1)
      return
      end function

      function aiscpri2(n,a,la,i,x,b,lr,li,m1)  
      implicit REAL*8 (a-h,r-z), integer (i-q)
*bug      dimension a(la,0:*),x(*),lr(*),li(*)
      dimension a(*),x(*),lr(*),li(*),la(*)
c  indirectly addressed scalar product with column i of A_2
*bug      aiscpri2=scpri(b,a(1,i),lr(m1+1),x(m1+1),n-m1)
      aiscpri2=scpri(b,a(i*la(1)+1),lr(m1+1),x(m1+1),n-m1)
      return
      end function

      function ailen(n,a,la,i)  
      implicit REAL*8 (a-h,r-z), integer (i-q)
*bug      dimension a(la,0:*)
       dimension a(*),la(*)
c  L2 length of column i of A
*bug      ailen=scpr(0.D0,a(1,i),a(1,i),n)
       ailen=scpr(0.D0,a(i*la(1)+1),a(i*la(1)),n)
      ailen=sqrt(ailen)
      return
      end function

      subroutine iscatter(a,la,i,li,an,n)  
      implicit REAL*8 (a-h,r-z), integer (i-q)
*bug      dimension a(la,0:*),li(*),an(*)
      dimension a(*),li(*),an(*),la(*)
c  indirect scatter into vector an
 
*bug     do j=1,n
*        an(li(j))=a(j,i)
*      enddo
*new:
       jbasis=i*la(1)
      do j=1,n
        an(li(j))=a(jbasis+j)
      enddo
      return
      end subroutine

      subroutine iunscatter(a,la,i,li,an,n)  
      implicit REAL*8 (a-h,r-z), integer (i-q)
*bug      dimension a(la,0:*),li(*),an(*)
       dimension a(*),li(*),an(*),la(*)
c  included for compatibility with sparseA.f
      return
      end subroutine

      function aij(i,j,a,la)  
      implicit REAL*8 (a-h,r-z), integer (i-q)
      dimension a(la,0:*)
c  get element A(i,j)
      aij=a(i,j)
      return
      end function

      subroutine isaxpy(a,x,lr,y,n)  
      implicit REAL*8 (a-h,r-z), integer (i-q)
      dimension x(*),lr(*),y(*)
c  saxpy with x indirectly addressed
      if(a.eq.0.D0)return
      do i=1,n
        y(i)=y(i)+a*x(lr(i))
      enddo
*opt riski       y(1:n)=y(1:n)+a*x(lr(1:n))
      return
      end subroutine

      function dscpr(a,x,y,n)  
      implicit REAL*8 (a-h,r-z), integer (i-q)
      dimension x(*),y(*)
      DOUBLE PRECISION dscpr
      dscpr=dble(a)
      do i=1,n
        dscpr=dscpr+dble(x(i))*dble(y(i))
      enddo
      return
      end function

      function scpri(a,x,lr,y,n)
      implicit REAL*8 (a-h,r-z), integer (i-q)
      dimension x(*),lr(*),y(*)
c  scpr with x indirectly addressed
      scpri=a
      do i=1,n
        scpri=scpri+x(lr(i))*y(i)
      enddo
*opt riski       scpri=a+dot_product(x(lr(1:n)),y(1:n))
      return
      end function

      function dscpri(a,x,lr,y,n)
      implicit REAL*8 (a-h,r-z), integer (i-q)
      dimension x(*),lr(*),y(*)
      DOUBLE PRECISION dscpri
      dscpri=dble(a)
      do i=1,n
        dscpri=dscpri+dble(x(lr(i)))*dble(y(i))
      enddo
      return
      end function

      subroutine cscale(n,m,a,la,x,bl,bu,s,menu,ifail)  
      implicit REAL*8 (a-h,r-z), integer (i-q)
      dimension a(la,0:*),x(*),bl(*),bu(*),s(*)

c     Constraint scaling procedure for use prior to calling bqpd when using
c     denseA.f

c     The user must set the parameter menu to control how the
c     x-variables are scaled (or equivalently how constraints i = 1:n
c     are scaled), as follows

c     menu = 1 indicates that a unit scaling applies to the x-variables

c     menu = 2 the user provides estimates s(i)>0 of the magnitude of
c              x(i) for i = 1:n. In this case the elements  x(i), bl(i), bu(i)
c              are divided by s(i) for i = 1:n.

c     In all cases, cscale goes on to scale the general constraints, in
c     such a way that the normal vector of each nontrivial constraint in
c     the scaled problem has an l_2 norm of unity. This scaling is also
c     applied to the right hand sides  bl(i), bu(i) for i = n+1:n+m.
c     The scaled data overwrites the original data.

c     cscale also scales the constant vector of the quadratic function,
c     which is found in a(1:n). However if a non-unit x-variable scaling
c     is used, it is necessary for the user to scale the Hessian matrix
c     G appropriately. This can be done by passing the x-variable scale
c     factors s(i) i = 1:n into the subroutine gdotx using the
c     parameter ws, and multiplying G(i,j) by s(i)*s(j) (possibly
c     implicitly).

c     cscale sets ifail = 1 to indicate that some s(i)< = 0,
c             and ifail = 2 to indicate an incorrect setting of menu.
c       Otherwise ifail = 0.

      ifail=2
      if(menu.lt.1.or.menu.gt.2)return
c     z=1.E0/log(2.E0)
      if(menu.eq.1)then
        do j=1,n
          s(j)=1.D0
        enddo
      else
        ifail=1
        do j=1,n
          if(s(j).le.0.D0)return
        enddo
c       if(menu.eq.2)then
c         do j=1,n
c           s(j)=2.E0**nint(log(s(j))*z)
c         enddo
c       endif
        do j=1,n
          if(s(j).ne.1.D0)then
            x(j)=x(j)/s(j)
            bl(j)=bl(j)/s(j)
            bu(j)=bu(j)/s(j)
            a(j,0)=a(j,0)*s(j)
          endif
        enddo
      endif
      do i=1,m
        t=0.D0
        do j=1,n
          a(j,i)=a(j,i)*s(j)
          t=t+a(j,i)**2
        enddo
        t=sqrt(t)
        if(t.eq.0.D0)then
          s(n+i)=1.D0
        else
c         t=2.E0**nint(log(t)*z)
          s(n+i)=t
          do j=1,n
            a(j,i)=a(j,i)/t 
          enddo
          bl(n+i)=bl(n+i)/t
          bu(n+i)=bu(n+i)/t
        endif
      enddo
      ifail=0
      return
      end subroutine
*******end of denseA 
christen this file denseL.f

!*bugs in several places:
!      call mysaxpy(sn(i),T(ij),sn,i-1)
! if i.eq.1 then ij may not be legal value (array bounds !!)



cut here >>>>>>>>>>>>>>>>>
c***************** dense matrix routines for manipulating L ********************

*there was call to ishift which casuyed problems
* caalling now lsshift which is in file auxil.f
* in epsol there was also array bound error 
*search *bug

c  ***************************************************************
c  Basis matrix routines for bqpd with dense matrices (block form)
c  ***************************************************************

c  These routines form and update L-Implicit-U factors LPB=U of a matrix B
c  whose columns are the normal vectors of the active constraints. In this
c  method only the unit lower triangular matrix L and the diagonal of U (in
c  addition to the row permutation P) is stored. B is represented in block form

c    | A_1  0 |    where the first m1 columns (A_1 and A_2) come from the
c    | A_2  I |    general constraint normals (columns of the matrix A in bqpd)

c  and the remaining unit columns come from simple bounds. The matrix A may be
c  specified in either dense or sparse format and the user is referred to the
c  files  denseA.f  or  sparseA.f. About m1*m1/2 locations are required to store
c  L-Implicit-U factors of B. The user MUST supply an upper bound on m1 by
c  setting mxm1 in the labelled common block

c     common/mxm1c/mxm1

c  Setting  mxm1=min(m+1,n)  is always sufficient.

c  Workspace
c  *********
c  denseL.f requires
c     mxm1*(mxm1+1)/2+3*n+mxm1   locations of real workspace, and
c     n+mxm1+n+m                 locations of integer workspace
c  These are stored at the end of the workspace arrays ws and lws in bqpd.
c  The user MUST set the lengths of these arrays in mxws and mxlws in
c     common/wsc/kk,ll,kkk,lll,mxws,mxlws
c  along with the values kk and ll of space to be used by gdotx.

c  Other information
c  *****************

c  L-Implicit-U factors are updated by a variant of the Fletcher-Matthews
c  method, which has proved very reliable in practice. The method is described
c  in the reference
c    Fletcher R., Dense Factors of Sparse Matrices, in "Approximation Theory
c    and Optimization. Tributes to M.J.D. Powell", (M.D. Buhmann and A. Iserles,
c    eds), Cambridge University Press (1997), pp. 145-166.

c  Steepest edge coefficients e(i) are also updated in these routines

c  The file contains routines for solving systems with B or its transpose
c  which might be of use in association with bqpd. These routines are
c  documented below.

c  Copyright, University of Dundee (R.Fletcher), June 1996
c  Current version dated 21/05/98

      subroutine start_up(n,nm,nmi,a,la,nk,e,ls,aa,ll,mode,ifail)  
      implicit REAL*8 (a-h,r-z), integer (i-q)
      dimension a(*),la(*),e(*),ls(*),aa(*),ll(*)
      common/noutc/nout
      common/wsc/kk,ll_,kkk,lll,mxws,mxlws
      common/epsc/eps,tol,emin
      common/densec/ns,ns1,nt,nt1,nu,nu1,mx1,lc,lc1,li,li1
      common/factorc/m0,m1,mm0,mm,mp,mq
      common/mxm1c/mxm1
      if(mxm1.le.0)then
        write(nout,*)'mxm1 =',mxm1,' is not set correctly'
        ifail=4
        return
      endif
      ns=kk+kkk+mxm1*(mxm1+1)/2+3*n+mxm1
      nt=ll_+lll+n+mxm1+nmi
!	write(6,*)'you gave real space ',mxws, 'needed',ns
 !     write(6,*)'you gave int space ',mxlws, 'needed',nt,
 !    3	'(ll_,lll,n,mxm1,nmi)',ll_,lll,n,mxm1,nmi
      if(ns.gt.mxws.or.nt.gt.mxlws)then
        write(*,*)'not enough real (ws) or integer (lws) workspace'
        write(*,*)'you give values for mxws and mxlws as',mxws,mxlws
        write(*,*)'minimum values for mxws and mxlws are',ns,nt
        ifail=7
        return
      endif
      small=max(1.D1*tol,sqrt(eps))
      smallish=max(eps/tol,1.D1*small)
c  set storage map for dense factors
      ns=mxm1*(mxm1+1)/2
      ns1=ns+1
      nt=ns+n
      nt1=nt+1
      nu=nt+n
      nu1=nu+1
      mx1=nu1+n
      lc=n
      lc1=lc+1
      li=lc+mxm1
      li1=li+1
c     write(nout,*)'ls',(ls(ij),ij=1,nk)
c     write(nout,*)'ls',(ls(ij),ij=nm+1,nmi)
      if(mode.ge.3)then
        call re_factor(n,nm,a,la,aa,aa(ns1),aa(nt1),ll,ll(lc1),ll(li1))
        call check_L(n,aa,ifail)
        if(ifail.eq.1)then
          mode=2
          goto1
        endif
        if(nk.eq.n)return
c  reset ls from e
        do j=1,nk
          i=-ls(j)
          if(i.gt.0)e(i)=-e(i)
        enddo
        j=0
        nk=nmi
        do i=1,nmi
          if(e(i).ne.0.D0)then
            j=j+1
            if(e(i).gt.0.D0)then
              ls(j)=i
            else
              ls(j)=-i
              e(i)=-e(i)
            endif
          else
            ls(nk)=i
            nk=nk-1
          endif
        enddo
        if(j.ne.n)then
          write(nout,*)'malfunction in reset sequence in start_up'
          stop
        endif
        ifail=0
        return
      endif
    1 continue
      if(emin.eq.0.D0)then
c  set a lower bound on e(i)
        emin=1.D0
        do i=1,nmi-n
          emin=max(emin,ailen(n,a,la,i))
        enddo
        emin=1.D0/emin
      endif
      do i=1,n
        e(i)=1.D0
        ll(i)=i
      enddo
      do i=n+1,nmi
        e(i)=0.D0
        ll(li+i)=0
      enddo
c  shift designated bounds to end
      nn=n
      do j=nk,1,-1
        i=abs(ls(j))
        if(i.eq.0.or.i.gt.nmi)then
          write(nout,*)
     *      'ls(j) is zero, or greater in modulus than n+m, for j =',j
          ifail=4
          return
        endif
        if(i.le.n)then
          ls(j)=ls(nk)
          nk=nk-1
          call iexch(ll(nn),ll(i))
          nn=nn-1
        endif
      enddo
      do i=1,n
        ll(li+ll(i))=i
      enddo
      m0=(max(mxm1-nk,0))/2
      mm0=m0*(m0+1)/2
      m1=0
      mm=mm0
      j=1
    2 continue
        if(j.gt.nk)goto3
        q=abs(ls(j))
c  extend factors
        call aqsol(n,a,la,q,aa,aa(nt1),aa(mx1),aa,ll,ll(lc1),ll(li1))
        m1p=m1+1
        call linf(nn-m1,aa(nt+m1p),z,iz)
        iz=iz+m1
        if(z.le.tol)then
c         write(nout,*)'reject c/s',q
          nk=nk-1
          do ij=j,nk
            ls(ij)=ls(ij+1)
          enddo
          goto2
        endif
        if(m1p.gt.mxm1)then
          write(nout,*)'mxm1 =',mxm1,'  is insufficient'
          ifail=7
          return
        endif
        if(iz.gt.m1p)then
c  pivot interchange
          ll(li+ll(m1p))=iz
          call iexch(ll(m1p),ll(iz))
          call rexch(aa(nt+m1p),aa(nt+iz))
          ll(li+ll(m1p))=m1p
        endif
        p=ll(m1p)
        tp=aa(nt+m1p)
        call eptsol(n,a,la,p,a,aa,aa(ns1),aa(nt1),ll,ll(lc1),ll(li1))
        aa(ns+m1p)=1.D0
c  update steepest edge coefficients
        ep=e(p)
c       eq=ep/tp
        eq=abs(ep/tp)
        tp_=tp/ep
        tpsq=tp_**2
        call aqsol(n,a,la,-1,a,aa(nu1),aa(mx1),aa,ll,ll(lc1),ll(li1))
        do i=1,m1p
          aa(nu+i)=aa(ns+i)/ep
        enddo
        do i=m1p+1,n
          aa(nu+i)=0.D0
        enddo
        e(p)=0.D0
        do i=1,nmi
          if(e(i).gt.0.D0)then
            ij=ll(li+i)
            ei=e(i)
c           ti=aa(nt+ij)*eq/ei
c           e(i)=max(emin,ei*sqrt(max(1.E0-ti*(2.E0*aa(nu+ij)/ei-ti),0.E0)))
            ti=aa(nt+j)/ei
            e(i)=max(emin,
     *        ei*sqrt(max(tpsq-ti*(2.D0*tp*aa(nu+j)/ei-ti),0.D0))*eq)
          endif
        enddo
c       e(q)=max(emin,abs(eq))
        e(q)=max(emin,eq)
        m1=m1p
        mm=mm+m0
        do ij=1,m1
          aa(mm+ij)=aa(ns+ij)
        enddo
        ll(lc+m1)=q
        ll(li+q)=m1
        mm=mm+m1
        aa(mm)=tp
        j=j+1
        goto2
    3 continue
c  complete the vector ls
      do i=nn+1,n
        nk=nk+1
        ls(nk)=ll(i)
      enddo
      j=nk
      do i=m1+1,nn
        j=j+1
C	write(6,*)'j,i',j,i
        ls(j)=ll(i)
      enddo
C	write(6,*)'lsmax',j,'llmax',nn,'m1',m1 !per
      do j=nm+1,nmi
        e(abs(ls(j)))=1.D0
      enddo
      j=n
      do i=1,nmi
        if(e(i).eq.0.D0)then
          j=j+1
          ls(j)=i
        endif
      enddo
      do j=nm+1,nmi
        e(abs(ls(j)))=0.D0
      enddo
      if(mode.gt.2)then
        z=sqrt(eps)
        do j=1,n
          i=abs(ls(j))
          e(i)=max(z,e(i))
        enddo
        do j=n+1,nmi
          i=abs(ls(j))
          e(i)=0.D0
        enddo
      endif
c     write(nout,*)'e =',(e(ij),ij=1,nmi)
c     write(nout,*)'PAQ factors'
c     ij=mm0+m0
c     do ii=1,m1
c       write(nout,*)(aa(ij+j),j=1,ii)
c       ij=ij+m0+ii
c     enddo
c     write(nout,*)'m0,mm0,m1,mm',m0,mm0,m1,mm
c     write(nout,*)'ls',(ls(ij),ij=1,nmi)
c     write(nout,*)'row perm',(ll(ij),ij=1,n)
c     write(nout,*)'column perm',(ll(lc+ij),ij=1,m1)
c     write(nout,*)'inverse perm',(ll(li+ij),ij=1,nmi)
c     call checkout(n,a,la,aa,ll,ll(lc1),ll(li1))
      mp=-1
      mq=-1
      ifail=0
      return
      end subroutine

      subroutine refactor(n,nm,a,la,aa,ll,ifail)  
      implicit REAL*8 (a-h,o-z)
      dimension a(*),la(*),aa(*),ll(*)
      common/densec/ns,ns1,nt,nt1,nu,nu1,mx1,lc,lc1,li,li1
      common/factorc/m0,m1,mm0,mm,mp,mq
 !     write(6,*)'******refactor'
!	read(5,*)iper
      call re_factor(n,nm,a,la,aa,aa(ns1),aa(nt1),ll,ll(lc1),ll(li1))
      call check_L(n,aa,ifail)
      return
      end subroutine

      subroutine pivot(p,q,n,nm,a,la,e,aa,ll,ifail,info)  
      implicit REAL*8 (a-h,r-z), integer (i-q)
      dimension a(*),la(*),e(*),aa(*),ll(*),info(*)
      common/noutc/nout
      common/iprintc/iprint
      common/densec/ns,ns1,nt,nt1,nu,nu1,mx1,lc,lc1,li,li1
      common/factorc/m0,m1,mm0,mm,mp,mq
      common/mxm1c/mxm1
      common/refactorc/nup,nfreq
      common/epsc/eps,tol,emin
c     write(nout,*)'pivot: p,q =',p,q
      if(p.ne.mp)then       
        call eptsol(n,a,la,p,a,aa,aa(ns1),aa(nt1),ll,ll(lc1),ll(li1))
        e(p)=sqrt(scpr(0.D0,aa(ns1),aa(ns1),m1+1))
        mp=p
*       else
*        if(nout.ne.0)write(nout,*)'pivot using mp',mp
      
      endif
      if(q.ne.mq)then
       
        call aqsol(n,a,la,q,a,aa(nt1),aa(mx1),aa,ll,ll(lc1),ll(li1))
        mq=q
*      else
*        if(nout.ne.0)write(nout,*)'pivot using mq',mq
      
      endif

c  update steepest edge coefficients
      tp=aa(nt+ll(li+p))
      if(tp.eq.0.D0)tp=eps
      ep=e(p)
c     eq=ep/tp
      eq=abs(ep/tp)
      tp=tp/ep
      tpsq=tp**2
      do i=1,m1+1
        aa(nu+i)=aa(ns+i)/ep
      enddo
      do i=m1+2,n
        aa(nu+i)=0.D0
      enddo
      call aqsol(n,a,la,-1,a,aa(nu1),aa(mx1),aa,ll,ll(lc1),ll(li1))
c     write(nout,*)'row perm',(ll(ij),ij=1,n)
c     write(nout,*)'column perm',(ll(lc+ij),ij=1,m1)
c     write(nout,*)'s =',(aa(ns+ij),ij=1,n)
c     write(nout,*)'t =',(aa(nt+ij),ij=1,n)
c     write(nout,*)'u =',(aa(nu+ij),ij=1,n)
      e(p)=0.D0
      do i=1,nm
        if(e(i).gt.0.D0)then
          j=ll(li+i)
          ei=e(i)
c         ti=aa(nt+j)*eq/ei
c         e(i)=max(emin,ei*sqrt(max(1.E0-ti*(2.E0*aa(nu+j)/ei-ti),0.E0)))
          ti=aa(nt+j)/ei
          e(i)=max(emin,
     *      ei*sqrt(max(tpsq-ti*(2.D0*tp*aa(nu+j)/ei-ti),0.D0))*eq)
        endif
      enddo
c     e(q)=max(emin,abs(eq))
      e(q)=max(emin,eq)
      info(1)=info(1)+1
	!  write(19,*)'nup,nfreq,m1',nup,nfreq,m1
      if(nup.ge.nfreq.and.m1.ne.0)then
c  refactorize L
      
C C if(nout.ne.0)then
C C write(nout,*)'***REFAC,nup,nfreq,n,m1',nup,nfreq,n,m1
C C end if

       
        ip=ll(li+p)
        if(p.gt.n)then
          qq=ll(lc+m1)
          ll(lc+ip)=qq
          ll(li+qq)=ip
          m1=m1-1
          ll(li+p)=0
        else
          m1p=m1+1
          ll(ip)=ll(m1p)
          ll(li+ll(ip))=ip
          ll(m1p)=p
          ll(li+p)=m1p
        endif
        if(q.gt.n)then
          if(m1.eq.mxm1)then
            ifail=7
           
            return
          endif
          m1=m1+1
          ll(lc+m1)=q
          ll(li+q)=m1
        else
          iq=ll(li+q)
          m1p=m1+1
          ll(iq)=ll(m1p)
          ll(li+ll(iq))=iq
          ll(m1p)=q
          ll(li+q)=m1p
        endif
       !  write(19,*)'**refactor,nup,mp,mq',nup,mp,mq
        call re_factor(n,nm,a,la,aa,aa(ns1),aa(nt1),ll,ll(lc1),ll(li1))
*       if(nout.gt.0)write(16,*)'aft refactor,nup,mp,mq',nup,mp,mq
      else
c  update L
        nup=nup+1
        if(p.le.n)then
          if(m1.eq.mxm1)then
            ifail=7
            return
          endif
          call linf(m1,aa(ns1),z,iz)
          if(z.le.4.D0)then
            if(m0+m1.eq.mxm1)then
c             write(nout,*)'m0 + m1 = mxm1:  re-centre triangle'
              ii=mm0
              mo=m0
              m0=m0/2
              mm0=m0*(m0+1)/2
              mm=mm0
              do i=1,m1
                ii=ii+mo+i
                mm=mm+m0+i
                do j=1-i,0
                  aa(mm+j)=aa(ii+j)
                enddo
              enddo
            endif
            do i=1,m1
              aa(mm+m0+i)=aa(ns+i)
            enddo
            goto1
          endif
        endif
        call c_flma(n,a,la,p,aa,ll,ll(lc1),ll(li1))
    1   continue
        if(q.le.n)then
          call r_flma(n,a,la,q,aa,ll,ll(lc1),ll(li1))
        else
          m1=m1+1
          mm=mm+m0+m1
          aa(mm)=1.D0
          aa(mm)=aiscpri1(n,a,la,q-n,aa(mm-m1+1),0.D0,ll,ll(li1),m1)
          if(abs(aa(mm)).le.eps)aa(mm)=eps
          ll(lc+m1)=q
          ll(li+q)=m1
        endif
        mp=-1
        mq=-1
      endif
      call check_L(n,aa,ifail)
c     write(nout,*)'PAQ factors'
c     ij=m0+mm0
c     do ii=1,m1
c       write(nout,*)(aa(ij+j),j=1,ii)
c       ij=ij+m0+ii
c     enddo
c     write(nout,*)'m0,mm0,m1,mm',m0,mm0,m1,mm
c     write(nout,*)'row perm',(ll(ij),ij=1,n)
c     write(nout,*)'column perm',(ll(lc+ij),ij=1,m1)
c     write(nout,*)'inverse perm',(ll(li+ij),ij=1,nm)
c     call checkout(n,a,la,aa,ll,ll(lc1),ll(li1))
c     write(nout,*)'steepest edge coefficients',(e(ij),ij=1,nm)
c     emax=0.E0
c     do i=1,nm
c       if(e(i).gt.0.E0)then
c         call eptsol(n,a,la,i,a,aa,aa(ns1),aa(nt1),ll,ll(lc1),ll(li1))
c         ei=sqrt(scpr(0.E0,aa(ns1),aa(ns1),n))
c         emax=max(emax,abs(ei-e(i)))
c       endif
c     enddo
c     if(emax.ge.tol)
c    *  write(nout,*)'error in steepest edge coefficients =',emax
      return
      end subroutine

      subroutine fbsub(n,jmin,jmax,a,la,q,b,x,ls,aa,ll,save) 
      implicit REAL*8 (a-h,r-z), integer (i-q)
      logical save
      dimension a(*),la(*),b(*),x(*),ls(*),aa(*),ll(*)

c  solves a system  B.x=b

c  Parameter list
c  **************
c   n   number of variables (as for bqpd)
c   a,la   specification of QP problem data (as for bqpd)
c   jmin,jmax  (see description of ls below)
c   q   an integer which, if in the range 1:n+m, specifies that the rhs vector
c       b is to be column q of the matrix A of general constraint normals.
c       In this case the parameter b is not referenced by fbsub.
c       If q=0 then b is taken as the vector given in the parameter b.
c   b(n)  must be set to the r.h.s. vector b (but only if q=0)
c   x(n+m)  contains the required part of the solution x, set according to the
c       index number of that component (in the range 1:n for a simple bound and
c       n+1:n+m for a general constraint)
c   ls(*)  an index vector, listing the components of x that are required.
c       Only the absolute value of the elements of ls are used (this allows
c       the possibility of using of the contents of the ls parameter of bqpd).
c       Elements of x in the range abs(ls(j)), j=jmin:jmax are set by fbsub.
c       These contortions allow bqpd to be independent of the basis matrix code.
c   aa(*)  real storage used by the basis matrix code (supply the vector
c       ws(lu1) with ws as in the call of bqpd and lu1 as in common/bqpdc/...)
c   ll(*)  integer storage used by the basis matrix code (supply the vector
c       lws(ll1) with lws as in the call of bqpd and ll1 as in common/bqpdc/...)
c   save   indicates if fbsub is to save its copy of the solution for possible
c       future use. We suggest that the user only sets save = .false.

      common/noutc/nout
      common/densec/ns,ns1,nt,nt1,nu,nu1,mx1,lc,lc1,li,li1
      common/factorc/m0,m1,mm0,mm,mp,mq
c     write(nout,*)'fbsub  q =',q
      if(save)then
        if(q.ne.mq)then
          call aqsol(n,a,la,q,b,aa(nt1),aa(mx1),aa,ll,ll(lc1),ll(li1))
          mq=q
!         else
!          if(nout.ne.0)write(16,*)'**fbsub using old mq',mq
        endif
        do j=jmin,jmax
          i=abs(ls(j))
          x(i)=aa(nt+ll(li+i))
        enddo
      else
        call aqsol(n,a,la,q,b,aa(nu1),aa(mx1),aa,ll,ll(lc1),ll(li1))
        do j=jmin,jmax
          i=abs(ls(j))
          x(i)=aa(nu+ll(li+i))
        enddo
      endif
      return
      end subroutine

      subroutine tfbsub(n,a,la,p,b,x,aa,ll,ep,save)  
      implicit REAL*8 (a-h,r-z), integer (i-q)
      logical save
      dimension a(*),la(*),b(*),x(*),aa(*),ll(*)

c  solves a system  Bt.x=b

c  Parameter list
c  **************
c   n   number of variables (as for bqpd)
c   a,la   specification of QP problem data (as for bqpd)
c   p    an integer which, if in the range 1:n+m, specifies that the rhs vector
c        b is a unit vector appropriate to the position of p in the current
c        ordering. In this case b is not referenced by tfbsub.
c   b(n+m)  If p=0, this must be set to the r.h.s. vector b. Only the components
c        of b need be set, according to the index number of each component (in
c        the range 1:n for a simple bound and n+1:n+m for a general constraint)
c   x(n)  contains the solution x (in natural ordering)
c   aa(*)  real storage used by the basis matrix code (supply the vector
c       ws(lu1) with ws as in the call of bqpd and lu1 as in common/bqpdc/...)
c   ll(*)  integer storage used by the basis matrix code (supply the vector
c       lws(ll1) with lws as in the call of bqpd and ll1 as in common/bqpdc/...)
c   ep  if p.ne.0 and save is true, ep contains the l_2 length of x on exit
c   save  indicates if tfbsub is to save its copy of the solution for possible
c       future use. We suggest that the user only sets save = .false.

      common/noutc/nout
      common/densec/ns,ns1,nt,nt1,nu,nu1,mx1,lc,lc1,li,li1
      common/factorc/m0,m1,mm0,mm,mp,mq
c     write(nout,*)'tfbsub  p =',p
      if(save)then
        if(p.ne.mp)then
          call eptsol(n,a,la,p,b,aa,aa(ns1),aa(nt1),ll,ll(lc1),ll(li1))
          mp=p
        endif
        do i=1,n
          x(ll(i))=aa(ns+i)
        enddo
        if(p.gt.0)ep=sqrt(scpr(0.D0,aa(ns1),aa(ns1),m1+1))
      else
*       if(nout.ne.0)write(nout,*)'flet:nu,nu1,nt1,aa(nu+1:nu+n)',
*     4 nu,nu1,nt1,aa(nu+1:nu+n)
*                              T    sn      tn
        call eptsol(n,a,la,p,b,aa,aa(nu1),aa(nt1),ll,ll(lc1),ll(li1))
*        if(nout.ne.0)write(nout,*)'nu aft',nu,nu1
        do i=1,n
*         if(nout.ne.0)write(nout,*)'i,ll(i),a(nu+i)',i,ll(i),aa(nu+i)
          x(ll(i))=aa(nu+i)
        enddo
      endif
c     write(nout,*)'x =',(x(i),i=1,n)
      return
      end subroutine

      subroutine newg  
      common/factorc/m0,m1,mm0,mm,mp,mq
      mq=-1
      return
      end subroutine

c******** The following routines are internal to denseL.f **************

      subroutine re_factor(n,nm,a,la,T,sn,tn,lr,lc,li)  
      implicit REAL*8 (a-h,r-z), integer (i-q)
      dimension a(*),la(*),T(*),sn(*),tn(*),lr(*),lc(*),li(*)
      common/noutc/nout
      common/iprintc/iprint
      common/refactorc/nup,nfreq
      common/factorc/m0,m1,mm0,mm,mp,mq
      common/mxm1c/mxm1
      common/epsc/eps,tol,emin
      if(nout.ne.0)write(nout,*)'re_factor,m1',m1
      nup=0
      if(m1.eq.0)return
      m0=(mxm1-m1)/2
      mm0=m0*(m0+1)/2
c     write(nout,*)'row perm',(lr(ij),ij=1,n)
c     write(nout,*)'column perm',(lc(ij),ij=1,m1)
      do i=1,m1
        sn(i)=0.D0
      enddo
      mm=mm0
      do i=1,m1-1
        mm=mm+m0+i
        im=i-1
        i1=mm-im
        q=lc(i)-n
*       if(nout.ne.0)write(nout,*)'q,n in refac',q,n
        if(q.le.0)goto1
c  form L.a_q
        call iscatter(a,la,q,li,sn,n)
c       write(nout,*)'aq =',(sn(ij),ij=1,m1)
        jj=mm
        j1=i1
        do j=i,m1
          tn(j)=scpr(sn(j),T(j1),sn,im)
          j1=jj+m0+1
          jj=j1+j
        enddo
        call iunscatter(a,la,q,li,sn,n)
c       write(nout,*)'L.aq =',(tn(ij),ij=i,m1)
        call linf(m1-im,tn(i),z,iz)
        if(iz.gt.1)then
c  pivot interchange
          iz=iz-1
          call vexch(T(i1),T(i1+iz*(m0+i)+iz*(iz-1)/2),im)
          iz=iz+i
          call rexch(tn(i),tn(iz))
          li(lr(i))=iz
          call iexch(lr(i),lr(iz))
          li(lr(i))=i
        endif
        if(tn(i).eq.0.D0)tn(i)=eps
c  update L
        j1=i1+m0+i
        zz=-tn(i)
        do j=i+1,m1
          z=tn(j)/zz
!*bug
       if(i.gt.1)   call mysaxpy(z,T(i1),T(j1),i-1)
          T(j1+im)=z
c         write(nout,*)'L(j) =',(T(ij),ij=j1,j1+im)
          j1=j1+m0+j
        enddo
        T(mm)=-zz
      enddo
      mm=mm+m0+m1
      q=lc(i)-n
*      if(nout.ne.0)write(nout,*)'q2,n in refac',q,n
      if(q.le.0)goto1
      call iscatter(a,la,q,li,sn,n)
      T(mm)=scpr(sn(m1),T(mm-m1+1),sn,m1-1)
      if(T(mm).eq.0.D0)T(mm)=eps
c     write(nout,*)'PAQ factors'
c     ij=mm0+m0
c     do ii=1,m1
c       write(nout,*)(T(ij+j),j=1,ii)
c       ij=ij+m0+ii
c     enddo
c     write(nout,*)'m0,mm0,m1,mm',m0,mm0,m1,mm
c     write(nout,*)'row perm',(lr(ij),ij=1,n)
c     write(nout,*)'column perm',(lc(ij),ij=1,m1)
c     write(nout,*)'inverse perm',(li(ij),ij=1,nm)
c     call checkout(n,a,la,T,lr,lc1,li)
      mp=-1
      mq=-1
      return
    1 continue
      write(nout,*)'malfunction in re_factor:  i,lc(i) =',i,q+n
      stop
      end subroutine

      subroutine check_L(n,T,ifail)  
      implicit REAL*8 (a-h,r-z), integer (i-q)
      dimension T(*)
      common/noutc/nout
      common/factorc/m0,m1,mm0,mm,mp,mq
      common/epsc/eps,tol,emin
c     write(nout,*)'check_L'
      ifail=1
      kk=mm0
      dmin=1.d37   !poista
      do k=1,m1
        kk=kk+m0+k
       dmin=min(dmin,abs(T(kk)))  !poista
        if(abs(T(kk)).le.tol)then
           write(*,*)'check_L,',t(kk),' .le. tol = ',tol
           if(nout.ne.0)write(nout,*)'check_L,',t(kk),' .le. tol = ',tol
              return
        end if
      enddo
       if(nout.ne.0) write(nout,*)'****dmin =',dmin
      ifail=0
      return
      end subroutine

      subroutine aqsol(n,a,la,q,b,tn,xm,T,lr,lc,li)  
      implicit REAL*8 (a-h,r-z), integer (i-q)
      dimension a(*),la(*),b(*),tn(*),xm(*),T(*),lr(*),lc(*),li(*)
      common/noutc/nout
      common/factorc/m0,m1,mm0,mm,mp,mq
c     write(nout,*)'aqsol  q =',q
      if(q.gt.0)then
        do i=1,n
          tn(i)=0.D0
        enddo
        if(q.le.n)then
          tn(li(q))=1.D0
        else
c         call isaipy(1.E0,a,la,q-n,tn,n,lr,li)
          call iscatter(a,la,q-n,li,tn,n)
        endif
      elseif(q.eq.0)then
        do i=1,n
          tn(li(i))=b(i)
        enddo
      endif
c     write(nout,*)'tn =',(tn(i),i=1,n)
      ii=mm
      do i=m1,1,-1
        xm(i)=(scpr(tn(i),T(ii-i+1),tn,i-1))/T(ii)
        call isaipy(-xm(i),a,la,lc(i)-n,tn,n,lr,li)
        ii=ii-m0-i
      enddo
      do i=1,m1
        tn(i)=xm(i)
      enddo
c     write(nout,*)'tn =',(tn(i),i=1,n)
      return
      end subroutine

      subroutine eptsol(n,a,la,p,b,T,sn,tn,lr,lc,li) 
      implicit REAL*8 (a-h,r-z), integer (i-q)
      dimension a(*),la(*),b(*),T(*),sn(*),tn(*),lr(*),lc(*),li(*)
      common/noutc/nout
      common/iprintc/iprint
      common/epsc/eps,tol,emin
      common/factorc/m0,m1,mm0,mm,mp,mq
      common/ipivotc/ipivot
  
c     write(nout,*)'eptsol  p =',p
       
      if(p.gt.n)then
        pr=li(p)
        if(pr.le.0)goto1
        if(pr.ne.m1)then
          z=tn(pr)
          call rshift(tn(pr),m1-pr,1)
          tn(m1)=z
          call c_flma(n,a,la,p,T,lr,lc,li)
          m1=m1+1
          mm=mm+m0+m1
          li(p)=m1
          lc(m1)=p
          T(mm)=1.D0
          T(mm)=aiscpri1(n,a,la,p-n,T(mm-m1+1),0.D0,lr,li,m1)
          if(T(mm).eq.0.D0)T(mm)=eps
c         write(nout,*)'PAQ factors'
c         ij=m0+mm0
c         do ii=1,m1
c           write(nout,*)(T(ij+j),j=1,ii)
c           ij=ij+m0+ii
c         enddo
c         write(nout,*)'m0,mm0,m1,mm',m0,mm0,m1,mm
c         write(nout,*)'row perm',(lr(ij),ij=1,n)
c         write(nout,*)'column perm',(lc(ij),ij=1,m1)
c         write(nout,*)'inverse perm',(li(ij),ij=1,p)
c         call checkout(n,a,la,T,lr,lc,li)
        endif
        ii=mm-m1
        z=1.D0/T(mm)
        do i=1,m1-1
          sn(i)=T(ii+i)*z
        enddo
        sn(m1)=z
        do i=m1+1,n
          sn(i)=0.D0
        enddo
      else
        ii=m0+mm0
        if(p.eq.0)then
          do i=1,m1
            sn(i)=0.D0
          enddo
          do i=m1+1,n
            sn(i)=b(lr(i))
          enddo
          do i=1,m1
            ii=ii+i
            j=lc(i)
            sn(i)=-aiscpri(n,a,la,j-n,sn,-b(j),lr,li)/T(ii)
!*bug
        if(i.gt.1)    call mysaxpy(sn(i),T(ij),sn,i-1)
            ii=ii+m0
            ij=ii+1
          enddo
        else
          pr=li(p)
          if(pr.le.m1)goto1
          m1p=m1+1
          call iexch(lr(pr),lr(m1p))
          call iexch(li(lr(pr)),li(lr(m1p)))
          call rexch(tn(pr),tn(m1p))
          do i=1,n
            sn(i)=0.D0
          enddo
          sn(m1p)=1.D0
*         if(nout.ne.0)write(nout,*)'m1.m1p ',m1,m1p
          do i=1,m1
            ii=ii+i
            sn(i)=-aiscpri(n,a,la,lc(i)-n,sn,0.D0,lr,li)/T(ii)
!*bug     
!        call mysaxpy(sn(i),T(ij),sn,i-1) 
        if(i.gt.1)    call mysaxpy(sn(i),T(ij),sn,i-1)
*         if(nout.ne.0)write(nout,*)'i,ii,ij,T(ii),T(ij),sn(i)',
*     5 i,ii,ij,T(ii),T(ij),sn(i)
            ii=ii+m0
            ij=ii+1
          enddo
        endif
      endif
!      if(nout.ne.0)write(nout,*)'sn =',(sn(i),i=1,n)
      return
    1 continue
      write(*,*)'malfunction detected in eptsol: p =',p
      write(*,*)'**pivot=',ipivot
      stop
      end subroutine

      subroutine c_flma(n,a,la,q,T,lr,lc,li)  
      implicit REAL*8 (a-h,r-z), integer (i-q)
      dimension a(*),la(*),T(*),lr(*),lc(*),li(*)
      common/noutc/nout
      common/mxm1c/mxm1
      common/epsc/eps,tol,emin
      common/factorc/m0,m1,mm0,mm,mp,mq
      REAL*8 l21
c     write(nout,*)'c_flma: q =',q
      qc=li(q)
      if(q.gt.n)then
        if(qc.le.0)goto1
        call ishift(lc(qc),m1-qc,1)
        do j=qc,m1-1
          li(lc(j))=j
        enddo
        li(q)=0
        mm=mm-m1-m0
        m1=m1-1
      else
        if(qc.le.m1)goto1
        call iexch(lr(qc),lr(m1+1))
        call iexch(li(lr(qc)),li(lr(m1+1)))
*bug        call ishift(lr(2),m1,-1)  lshift added by j.l.
        call lshift(lr,2,m1,-1)
        lr(1)=q
        do i=1,m1+1
          li(lr(i))=i
        enddo
        if(m0.eq.0)then
c         write(nout,*)'m0 = 0:  re-centre triangle'
          m0=(mxm1+1-m1)/2
          mm0=m0*(m0+1)/2
          ii=mm
          mm=(m0+m1)*(m0+m1+1)/2
          ii=ii-mm
          ij=mm+m0+1
! bug 
         if(ii.ge.0)then
          do i=m1,1,-1
            ij=ij-m0-i
            call rshift(T(ij),i,ii)
            ii=ii+m0
          enddo
         else
            do i=m1,1,-1
            ij=ij-m0-i
            call rlshift(T,ij,i,ii)
            ii=ii+m0
          enddo


          end if  !by JL
        endif
        mm=mm-m0-m1
        m0=m0-1
        do i=1,m1
          mm0=mm0+m0+i
          T(mm0)=0.D0
        enddo
        mm0=m0*(m0+1)/2
        qc=1
      endif
      iswap=0
      ii=(qc+m0)*(qc+m0+1)/2
      do i=qc,m1
        im=i+m0
        ii1=ii+m0+1
        iip=ii1+i
        T(ii)=1.D0
        u21=T(iip)
        u11=aiscpri1(n,a,la,lc(i)-n,T(ii1-im),0.D0,lr,li,i)
        ij=ii+im-iswap
c       write(nout,*)'i,im,ii,iip,iswap,ij',i,im,ii,iip,iswap,ij
        l21=T(ij)
        if(abs(l21).le.eps)l21=0.D0
        if(iswap.gt.0)call rshift(T(ij),iswap,1)
        del=u21-l21*u11
c       write(nout,*)'l21,u11,u21,del =',l21,u11,u21,del
c       write(nout,*)'old row =',(T(j),j=ii1-im,ii)
c       write(nout,*)'new row =',(T(j),j=ii1,ii+im)
        if(abs(del).le.abs(u11)*max(1.D0,abs(l21)))then
c         if(u11.eq.0.E0)then
c           r=0.E0
c         else
            if(u11.eq.0.D0)u11=eps
            r=-u21/u11
            if(abs(r).le.eps)r=0.D0
!*bug 
      if(i.gt.1)      call mysaxpy(r,T(ii1-im),T(ii1),i-1)
c         endif
          T(ii)=u11
          T(ii+im)=l21+r
          if(iswap.gt.0)then
            do j=im+1,m0+m1
              ij=ij+j
              r=T(ij)
              call rshift(T(ij),iswap,1)
              T(ij+iswap)=r
            enddo
          endif
          iswap=0
        else
          r=-u11/del
          if(abs(r).le.eps)r=0.D0
*bug  *notify Fletcher
*bug    
      call permop(T(ii1-im),T(ii1),r,-l21,i-1)

!         call permop(T(ii1-im),T(ii1),r,dble(-l21),i-1)

          T(ii)=del
          T(ii+im)=r
          call iexch(lr(i),lr(i+1))
          call iexch(li(lr(i)),li(lr(i+1)))
          iswap=iswap+1
        endif
        ii=iip
      enddo
      return
    1 continue
      write(nout,*)'malfunction detected in c_flma: q =',q
      stop
      end subroutine

      subroutine r_flma(n,a,la,p,T,lr,lc,li)  
      implicit REAL*8 (a-h,r-z), integer (i-q)
      dimension a(*),la(*),T(*),lr(*),lc(*),li(*)
      common/noutc/nout
      common/epsc/eps,tol,emin
      common/factorc/m0,m1,mm0,mm,mp,mq
      REAL*8 l11
c     write(nout,*)'r_flma: p =',p
      pr=li(p)
      if(pr.gt.m1)then
        if(pr.eq.m1+1)return
        write(nout,*)'malfunction detected in r_flma: p =',p
        stop
      endif
      ii=(pr+m0)*(pr+m0+1)/2
      u11=T(ii)
      T(ii)=1.D0
      ip=ii
      do i=pr,m1-1
        im=i+m0
        ii1=ii+m0+1
        iip=ii1+i
        u22=T(iip)
        l11=-T(ip+im)/T(ip)
        if(abs(l11).le.eps)l11=0.D0
        u12=aiscpri1(n,a,la,lc(i+1)-n,T(ii1-im),0.D0,lr,li,i)
        del=l11*u12+u22
c       write(nout,*)'l11,u11,u12,u22,del',l11,u11,u12,u22,del
c       write(nout,*)'old row =',(T(j),j=ii1-im,ii)
c       write(nout,*)'new row =',(T(j),j=ii1,ii+im)
        if(abs(del).le.abs(l11)*max(abs(u11),abs(u12)))then
*bug notify
*bug  
        call saxpyx(l11,T(ii1-im),T(ii1),i)
!          call saxpyx(dble(l11),T(ii1-im),T(ii1),i)
          u11=l11*u11
          if(u11.eq.0.D0)u11=eps
          T(iip)=1.D0
        else
          r=-u12/del
          if(abs(r).le.eps)r=0.D0
*bug notify        
       call permop(T(ii1-im),T(ii1),r,l11,i)
 !         call permop(T(ii1-im),T(ii1),r,dble(l11),i)
          call iexch(lc(i),lc(i+1))
          call iexch(li(lc(i)),li(lc(i+1)))
          T(iip)=r
          u22=u11*u22/del
          u11=del
        endif
        call rshift(T(ip),i-pr,1)
        T(ii)=u11
        u11=u22
        ip=ip+im
        ii=iip
      enddo
      call ishift(lr(pr),m1-pr+1,1)
      lr(m1+1)=p
      do j=pr,m1+1
        li(lr(j))=j
      enddo
c     if(T(ip).eq.0.E0)T(ip)=eps
      l11=-T(ip+m0+m1)/T(ip)
*bug notify     
       call saxpyx(l11,T(mm-m1+1),T(mm+m0+1),m1)
 !           call saxpyx(dble(l11),T(mm-m1+1),T(mm+m0+1),m1)
      call rshift(T(ip),m1-pr,1)
      T(mm)=l11*u11
      if(T(mm).eq.0.D0)T(mm)=eps
      return
      end subroutine

      subroutine permop(v1,v2,r,s,n)  
      implicit REAL*8 (a-h,o-z)
      dimension v1(*),v2(*)
      common/noutc/nout
      if(s.eq.0)then
        if(r.eq.0)then
          call vexch(v1,v2,n)
        else
          do i=1,n
            z=v2(i)
            v2(i)=v1(i)+r*z
            v1(i)=z
          enddo
        endif
      else
        if(r.eq.0)then
          do i=1,n
            z=v1(i)
            v1(i)=v2(i)+s*z
            v2(i)=z
          enddo
        else
          do i=1,n
            z=v1(i)
            v1(i)=v2(i)+s*z
            v2(i)=z+r*v1(i)
          enddo
        endif
      endif
      return
      end subroutine

      subroutine checkout(n,a,la,T,lr,lc,li) 
      implicit REAL*8 (a-h,o-z)
      dimension a(*),la(*),T(*),lr(*),lc(*),li(*)
      common/noutc/nout
      common/mxm1c/mxm1
      common/epsc/eps,tol,emin
      common/factorc/m0,m1,mm0,mm,mp,mq
      emax=0.D0
      gmax=0.D0
      ii=mm0
      do i=1,m1
        ii1=ii+m0+1
        ii=ii+m0+i
        d=T(ii)
        T(ii)=1.D0
        do j=1,i-1
          e=aiscpri1(n,a,la,lc(j)-n,T(ii1),0.D0,lr,li,i)
          emax=max(emax,abs(e))
          gmax=max(gmax,abs(T(ii+m0+j)))
        enddo
        e=aiscpri1(n,a,la,lc(i)-n,T(ii1),-d,lr,li,i)
        emax=max(emax,abs(e))
        T(ii)=d
      enddo
c     if(emax.gt.tol.or.gmax.gt.1.E1)
c    *  write(nout,*)'error in LA=U is ',emax,'  growth in L =',gmax
      write(nout,*)'error in LA=U is ',emax,'  growth in L =',gmax
      return
      end subroutine
****   denseL
      end module

      module fletchersparse  
	use utilmod
	contains
c	christen this file    sparseA.f
cut here >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
c  ******************************************
c  Specification of A in sparse matrix format
c  ******************************************

c  The matrix A contains gradients of the linear terms in the objective
c  function (column 0) and the general constraints (columns 1:m).
c  No explicit reference to simple bound constraints is required in A.
c  The information is set in the parameters a (real) and la (integer) of bqpd.

c  In this sparse format, these vectors have dimension  a(1:nnza)  and
c   la(0:lamax), where nnza is the number of nonzero elements in A,
c  and lamax is at least  nnza+m+2.  The last m+2 elements in la are pointers.

c  The vectors a(.) and la(.) must be set as follows:

c  a(j) and la(j) for j=1,nnza are set to the values and row indices (resp.)
c  of all the nonzero elements of A. Entries for each column are grouped
c  together in increasing column order.

c  The last m+2 elements of la(.) contain pointers to the first elements in
c  the column groupings. Thus la(la(0)+i) for i=0,m is set to the location
c  in a(.) containing the first nonzero element for column i of A. Also
c  la(la(0)+m+1) is set to nnza+1 (the first unused location in a(.)).

c  Finally la(0) is also a pointer which points to the start of the pointer
c  information in la. la(0) must be set to nnza+1 (or a larger value if it
c  is desired to allow for future increases to nnza).

c  Copyright, University of Dundee (R.Fletcher), June 1996
c  Current version dated 21/05/98

      subroutine saipy(s,a,la,i,y,n)  
	use lastmod
      implicit REAL*8 (a-h,o-z)
      dimension a(*),la(0:*),y(*)
c  saxpy with column i of A
      if(s.eq.0.d0)return
      jp=la(0)+i
*jsp

      do j=la(jp),last(i) !la(jp+1)-1
        ir=la(j)
        y(ir)=y(ir)+s*a(j)
      enddo
      return
      end subroutine

c     subroutine daipy(s,a,la,i,y,n)
c     DOUBLE PRECISION a(*),y(*),d
c     dimension la(0:*)
c     if(s.eq.0.E0)return
c     d=dble(s)
c     jp=la(0)+i
c     do j=la(jp),la(jp+1)-1
c       ir=la(j)
c       y(ir)=y(ir)+d*dble(a(j))
c     enddo
c     return
c     end

      subroutine isaipy(s,a,la,i,y,n,lr,li)  
	use lastmod
      implicit REAL*8 (a-h,o-z)
      dimension a(*),la(0:*),y(*),lr(*),li(*)
c  indirectly addressed saxpy with column i of A
      if(s.eq.0.d0)return
*jsp
      jp=la(0)+i



      do j=la(jp),last(i) !la(jp+1)-1
        ir=li(la(j))
        y(ir)=y(ir)+s*a(j)
      enddo
      return
      end subroutine

c the old isaipy was what might be called isaipy2

      subroutine isaipy1(s,a,la,i,y,n,lr,li,m1)  
	use lastmod
      implicit REAL*8 (a-h,o-z)
      dimension a(*),la(0:*),y(*),lr(*),li(*)
c  indirectly addressed saxpy with column i of A_1
      if(s.eq.0.d0)return
      jp=la(0)+i
*jsp
 

      do j=la(jp),last(i) !la(jp+1)-1
        ir=li(la(j))
        if(ir.le.m1)y(ir)=y(ir)+s*a(j)
      enddo
      return
      end subroutine

c     subroutine ssaipy(s,a,la,i,y,n)
c     implicit REAL (a-h,o-z)
c     dimension a(*),la(0:*),y(*)
c  saxpy with squares of column i of A
c     if(s.eq.0.E0)return
c     jp=la(0)+i
c     do j=la(jp),la(jp+1)-1
c       ir=la(j)
c       y(ir)=y(ir)+s*(a(j))**2
c     enddo
c     return
c     end

      function aiscpr(n,a,la,i,x,b)
	use lastmod
      implicit REAL*8 (a-h,o-z)
      dimension a(*),la(0:*),x(*)
c  scalar product with column i of A
      aiscpr=b
*jsp
      jp=la(0)+i
      do j=la(jp),last(i) !la(jp+1)-1
        ir=la(j)
        aiscpr=aiscpr+x(ir)*a(j)
      enddo
      return
      end function

      function daiscpr(n,a,la,i,x,b)
	use lastmod
      implicit REAL*8 (a-h,o-z)
      dimension a(*),la(0:*),x(*)
      DOUBLE PRECISION daiscpr
      daiscpr=dble(b)
*jsp
      jp=la(0)+i
      do j=la(jp),last(i) !la(jp+1)-1
        ir=la(j)
*        daiscpr=daiscpr+dble(x(ir))*dble(a(j))  !6.4.2009
         daiscpr=daiscpr+x(ir)*a(j)
      enddo
      return
      end function

      function aiscpri(n,a,la,i,x,b,lr,li)
	use lastmod
      implicit REAL*8 (a-h,o-z)
      dimension a(*),la(0:*),x(*),lr(*),li(*)
c  indirectly addressed scalar product with column i of A
      aiscpri=b
*jsp
      jp=la(0)+i
      do j=la(jp),last(i) ! la(jp+1)-1
        ir=li(la(j))
        aiscpri=aiscpri+x(ir)*a(j)
      enddo
      return
      end function

      function daiscpri(n,a,la,i,x,b,lr,li)
	use lastmod
      implicit REAL*8 (a-h,o-z)
      dimension a(*),la(0:*),x(*),lr(*),li(*)
      DOUBLE PRECISION daiscpri
      daiscpri=dble(b)
      jp=la(0)+i
*jsp
 
      do j=la(jp),last(i) !la(jp+1)-1
        ir=li(la(j))
        daiscpri=daiscpri+dble(x(ir))*dble(a(j))
      enddo
      return
      end function

c the old aiscpri was what might be called aiscpri2

      function aiscpri1(n,a,la,i,x,b,lr,li,m1)
	use lastmod
      implicit REAL*8 (a-h,o-z)
      dimension a(*),la(0:*),x(*),lr(*),li(*)
c  indirectly addressed scalar product with column i of A_1
      aiscpri1=b
*jsp
      jp=la(0)+i
      do j=la(jp),last(i) ! la(jp+1)-1
        ir=li(la(j))
        if(ir.le.m1)aiscpri1=aiscpri1+x(ir)*a(j)
      enddo
      return
      end function

      function ailen(n,a,la,i)
	use lastmod
      implicit REAL*8 (a-h,o-z)
      dimension a(*),la(0:*)
c  L2 length of column i of A
      ailen=0.d0
*jsp
      jp=la(0)+i
      do j=la(jp),last(i) ! la(jp+1)-1
        ailen=ailen+a(j)**2
      enddo
      ailen=sqrt(ailen)
      return
      end function

      subroutine iscatter(a,la,i,li,an,n)  
	use lastmod
      implicit REAL*8 (a-h,o-z)
      dimension a(*),la(0:*),li(*),an(*)
c  indirect scatter into previously zeroed vector an
      jp=la(0)+i
*jsp
      do j=la(jp),last(i) !la(jp+1)-1
        an(li(la(j)))=a(j)
      enddo
      return
      end subroutine

      subroutine iunscatter(a,la,i,li,an,n)  
	use lastmod
      implicit REAL*8 (a-h,o-z)
      dimension a(*),la(0:*),li(*),an(*)
c  undo effect of iscatter
*jsp
      jp=la(0)+i
      do j=la(jp),last(i) !la(jp+1)-1
        an(li(la(j)))=0.d0
      enddo
      return
      end subroutine

      function aij(i,j,a,la)  
		use lastmod
      implicit REAL*8 (a-h,o-z)
      dimension a(*),la(0:*)
c  get element A(i,j)
*jsp
      jp=la(0)+j
      do jj=la(jp),last(j) ! la(jp+1)-1
        ir=la(jj)
        if(ir.eq.i)then
          aij=a(jj)
          return
        endif
      enddo
      aij=0.d0
      return
      end function

      subroutine cscale(n,m,a,la,x,bl,bu,s,menu,ifail)  
	use lastmod
      implicit REAL*8 (a-h,o-z)
      dimension a(*),la(0:*),x(*),bl(*),bu(*),s(*)

c     Constraint scaling procedure for use prior to calling bqpd when using
c     sparseA.f

c     Parameters are set as for bqpd, except for s, menu and ifail

c     The user must set the parameter menu to control how the
c     x-variables are scaled (or equivalently how constraints i = 1:n
c     are scaled), as follows

c     menu = 1 indicates that a unit scaling applies to the x-variables

c     menu = 2 the user provides estimates s(i)>0 of the magnitude of
c              x(i) for i = 1:n. In this case the elements  x(i), bl(i), bu(i)
c              are divided by s(i) for i = 1:n.

c     In all cases, cscale goes on to scale the general constraints, in
c     such a way that the normal vector of each nontrivial constraint in
c     the scaled problem has an l_2 norm of unity. This scaling is also
c     applied to the right hand sides  bl(i), bu(i) for i = n+1:n+m.
c     The scaled data overwrites the original data.

c     cscale also scales the constant vector of the quadratic function,
c     which is found in a(1:n). However if a non-unit x-variable scaling
c     is used, it is necessary for the user to scale the Hessian matrix
c     G appropriately. This can be done by passing the x-variable scale
c     factors s(i) i = 1:n into the subroutine gdotx using the
c     parameter ws, and multiplying G(i,j) by s(i)*s(j) (possibly
c     implicitly).

c     cscale sets ifail = 1 to indicate that some s(i)< = 0,
c             and ifail = 2 to indicate an incorrect setting of menu.
c       Otherwise ifail = 0.

      integer pjp

      ifail=2
      if(menu.lt.1.or.menu.gt.2)return
      pjp=la(0)
c     z=1.E0/log(2.E0)
      if(menu.eq.1)then
        do j=1,n
          s(j)=1.d0
        enddo
      else
        ifail=1
        do j=1,n
          if(s(j).le.0.d0)return
        enddo
c       if(menu.eq.2)then
c         do j=1,n
c           s(j)=2.E0**nint(log(s(j))*z)
c         enddo
c       endif
        do j=1,n
          if(s(j).ne.1.d0)then
            x(j)=x(j)/s(j)
            bl(j)=bl(j)/s(j)
            bu(j)=bu(j)/s(j)
          endif
        enddo
	!pjp= la(0)  eka
        do j=1,last(0) !la(pjp+1)-1
          a(j)=a(j)*s(la(j))
        enddo
      endif
      do i=1,m
        t=0.d0
*jsp
        do j=la(pjp+i),last(i) !la(pjp+i+1)-1
          a(j)=s(la(j))*a(j)
          t=t+a(j)**2
        enddo
        t=sqrt(t)
        if(t.eq.0.d0)then
          s(n+i)=1.d0
        else
c         t=2.E0**nint(log(t)*z)
          s(n+i)=t
*jsp
          do j=la(pjp+i),last(i) ! la(pjp+i+1)-1
            a(j)=a(j)/t 
          enddo
          bl(n+i)=bl(n+i)/t
          bu(n+i)=bu(n+i)/t
        endif
      enddo
      ifail=0
      return
      end subroutine
************************************************************************************
c	christen this file sparseL.f
cut here >>>>>>>>>>>>>>>>>
c***************** sparse matrix routines for manipulating L *******************

c           ***************************************************
c           Basis matrix routines for bqpd with sparse matrices
c           ***************************************************

c  These routines form and update L-Implicit-U factors LPB=U of a matrix B
c  whose columns are the normal vectors of the active constraints. In this
c  method only the unit lower triangular matrix L and the diagonal of U (in
c  addition to the row permutation P) is stored. B is represented in block form

c    | I  A_2 |    where the last m1 columns (A_2 and A_1) come from the
c    | 0  A_1 |    general constraint normals (columns of the matrix A in bqpd)

c  and the remaining unit columns come from simple bounds. The matrix A must be
c  specified in sparse format and the user is referred to the file  sparseA.f.

c  The data structure used for L is that of a profile or skyline scheme, in
c  which the nontrivial rows of L are stored as dense row spikes. The use of
c  a Tarjan+spk1 ordering algorithm to control the length of these spikes has
c  proved quite effective. The factors are updated by a variant of the
c  Fletcher-Matthews method, which has proved very reliable in practice.
c  However the B matrix is re-factored every 30 updates to control growth in
c  the total spike length.

c  Workspace
c  *********
c  The user needs to supply storage for the rows of L, although the amount
c  required is unknown a-priori.
c  sparse.f requires
c     5*n+nprof          locations of real workspace, and
c     9*n+m              locations of integer workspace
c  where nprof is the space required for storing the row spikes of the L matrix.
c  Storage for sparseL.f is situated at the end of the workspace arrays ws
c  and lws in bqpd.
c  Allow as much space for nprof as you can afford: the routine will report if
c  there is not enough. So far 10^6 locations has proved adequate for problems
c  of up to 5000 variables.

c  In addition the current version of bqpd.f requires
c     kmax*(kmax+9)/2+2*n+m   locations of real workspace in ws
c     kmax                    locations of integer workspace in lws
c  The user is also allowed to reserve storage in ws and lws, for use in the
c  user-supplied routine gdotx. This storage is situated at the start of the
c  arrays ws and lws. The user specifies the amount required by
c  setting the parameters kk and ll in the common block
c     common/wsc/kk,ll,kkk,lll,mxws,mxlws
c  The user MUST also set mxws and mxlws to be (respectively) the total amount
c  of real and integer workspace for the arrays ws and lws.

c  Other information
c  *****************

c  The methodology behind the L-Implicit-U factors and the row spike storage
c  scheme for L is described in the references
c    Fletcher R., Dense Factors of Sparse Matrices, in "Approximation Theory
c    and Optimization. Tributes to M.J.D. Powell", (M.D. Buhmann and A. Iserles,
c    eds), Cambridge University Press (1997), pp. 145-166.
c  and
c    Fletcher R., Block Triangular Orderings and Factors for Sparse Matrices
c    in LP, in "Numerical analysis 1997" (D.F. Griffiths, D.J. Higham and
c    G.A. Watson, eds.), Pitman Research Notes in Mathematics 380, (1998),
c    Longman, Harlow, pp. 91-110.

c  The file contains routines for solving systems with B or its transpose
c  which might be of use in association with bqpd. These routines are
c  documented below.

c  Steepest edge coefficients e(i) are also updated in these routines

c  Copyright, University of Dundee (R.Fletcher), January 1998
c  Current version dated 14/10/99

      subroutine start_up(n,nm,nmi,a,la,nk,e,ls,aa,ll,mode,ifail) 
      implicit REAL*8 (a-h,r-z), integer (i-q)
      dimension a(*),la(0:*),e(*),ls(*),aa(*),ll(*)
      common/noutc/nout
      common/wsc/kk,ll_,kkk,lll,mxws,mxlws
      common/epsc/eps,tol,emin
      common/sparsec/ns,ns1,nt,nt1,nu,nu1,nx,nx1,np,np1,nprof,
     *  lc,lc1,li,li1,lm,lm1,lp,lp1,lq,lq1,lr,lr1,ls_,ls1,lt,lt1
      common/factorc/m1,m2,mp,mq,lastr,irow
      common/refactorc/nup,nfreq
        nfreq=min(30,nfreq)
      ns=kk+kkk+5*n
      nt=ll_+lll+8*n+nmi
      nprof=mxws-ns
!		write(6,*)'you gave real space ',mxws, 'needed',ns
!      write(6,*)'you gave int space ',mxlws, 'needed',nt,
 !    3	'(ll_,lll,n,nmi)',ll_,lll,n,nmi
      if(nprof.le.0.or.nt.gt.mxlws)then
        write(*,*)'not enough real (ws) or integer (lws) workspace'
        write(*,*)'you give values for mxws and mxlws as',mxws,mxlws
        write(*,*)'minimum values for mxws and mxlws are',ns,nt
        ifail=7
        return
      endif
c  set storage map for sparse factors
      ns=n
      ns1=ns+1
      nt=ns+n
      nt1=nt+1
      nu=nt+n
      nu1=nu+1
      nx=nu+n
      nx1=nx+1
      np=nx+n
      np1=np+1
      lc=n
      lc1=lc+1
      li=lc+n
      li1=li+1
      lm=li+nmi
      lm1=lm+1
      lp=lm+n
      lp1=lp+1
      lq=lp+n
      lq1=lq+1
      lr=lq+n
      lr1=lr+1
      ls_=lr+n
      ls1=ls_+1
      lt=ls_+n
      lt1=lt+1
      m=nm-n
      mp=-1
      mq=-1
c     write(nout,*)'ls',(ls(ij),ij=1,nk)
      if(mode.ge.3)then
        call re_order(n,nm,a,la(1),la(la(0)),ll,ll(lc1),ll(li1),
     *    ll(lm1),ll(lp1),ll(lq1),ll(lr1),ll(ls1),ll(lt1),aa(np1),
     *    nprof,ifail)
        if(ifail.ge.1)then
          write(nout,*)'failure in re_order (1)'
          if(ifail.eq.7)return
          mode=2
          goto1
        endif
		write(6,*)'refac'
        call re_factor(n,nm,a,la,ll,ll(lc1),ll(li1),
     *    ll(lm1),ll(lp1),ll(lq1),ll(lr1),ll(ls1),ll(lt1),aa(np1),
     *    nprof,aa,ifail)
        if(ifail.eq.7)return
        call check_L(n,aa,ll(lp1),ifail)
        if(ifail.eq.1)then
          mode=2
          goto1
        endif
        if(nk.eq.n)return
c  reset ls from e
        do j=1,nk
          i=-ls(j)
          if(i.gt.0)e(i)=-e(i)
        enddo
        j=0
        nk=nmi
        do i=1,nmi
          if(e(i).ne.0.d0)then
            j=j+1
            if(e(i).gt.0.d0)then
              ls(j)=i
            else
              ls(j)=-i
              e(i)=-e(i)
            endif
          else
            ls(nk)=i
            nk=nk-1
          endif
        enddo
        if(j.ne.n)then
          write(*,*)'malfunction in reset sequence in start_up'
          stop
        endif
        return
      endif
    1 continue
      if(emin.eq.0.d0)then
c  set a lower bound on e(i)
        emin=1.d0
        do i=1,nmi-n
          emin=max(emin,ailen(n,a,la,i))
        enddo
        emin=1.d0/emin
      endif
      do i=1,n
        ll(i)=i
        ll(li+i)=i
        e(i)=1.d0
      enddo
      do i=n+1,nmi
        ll(li+i)=0
        e(i)=0.d0
      enddo
      nu_=0
      if(mode.ne.0)then
c  shift designated bounds to end and order the resulting rows and columns
        do j=1,nk
          i=abs(ls(j))
          if(i.le.n)then
            nn=n-nu_
            nu_=nu_+1
            call iexch(ls(nu_),ls(j))
            ii=ll(li+i)
            ll(ii)=ll(nn)
            ll(li+ll(ii))=ii
            ll(nn)=i
            ll(li+i)=nn
          endif
        enddo
        call order(n,nu_,nk,la,ll,ls,ll(li1),ll(lp1),ll(lq1),ll(lr1),
     *    aa(np1),nprof,ifail)
        if(ifail.gt.0)return
      endif
      call factor(n,nmi,nu_,nk,a,la,e,ls,aa(ns1),aa(nt1),aa(nu1),
     *  aa(nx1),ll,ll(lc1),ll(li1),ll(lm1),ll(lp1),ll(lq1),ll(lr1),
     *  ll(ls1),aa(np1),nprof,aa,ifail)
      if(ifail.gt.0)return
c     write(nout,*)'steepest edge coefficients',(e(ij),ij=1,nm)
c     emax=0.E0
c     do i=1,nm
c       if(e(i).gt.0.E0)then
c         call eptsol(n,a,la,i,a,aa(ns1),aa(nt1),aa,aa(np1),
c    *      ll,ll(lc1),ll(li1),ll(lp1),ll(lq1))
c         ei=xlen(0.E0,aa(ns1),n)
c         ei=sqrt(scpr(0.E0,aa(ns1),aa(ns1),n))
c         emax=max(emax,abs(ei-e(i)))
c       endif
c     enddo
c     if(emax.ge.tol)
c    *  write(nout,*)'error in steepest edge coefficients =',emax
      return
      end subroutine

      subroutine refactor(n,nm,a,la,aa,ll,ifail)  
      implicit REAL*8 (a-h,o-z)
      dimension a(*),la(0:*),aa(*),ll(*)
      common/sparsec/ns,ns1,nt,nt1,nu,nu1,nx,nx1,np,np1,nprof,
     *  lc,lc1,li,li1,lm,lm1,lp,lp1,lq,lq1,lr,lr1,ls,ls1,lt,lt1
      common/factorc/m1,m2,mp,mq,lastr,irow
      common/noutc/nout
!      write(6,*)'***refactor'
!       read(5,*)iperk
      m=nm-n
      call re_order(n,nm,a,la(1),la(la(0)),ll,ll(lc1),ll(li1),
     *  ll(lm1),ll(lp1),ll(lq1),ll(lr1),ll(ls1),ll(lt1),aa(np1),
     *  nprof,ifail)
      if(ifail.ge.1)then
        write(nout,*)'failure in re_order (2)'
        return
      endif
      call re_factor(n,nm,a,la,ll,ll(lc1),ll(li1),ll(lm1),
     *  ll(lp1),ll(lq1),ll(lr1),ll(ls1),ll(lt1),aa(np1),
     *  nprof,aa,ifail)
      if(ifail.eq.7)return
      call check_L(n,aa,ll(lp1),ifail)
      return
      end subroutine

      subroutine pivot(p,q,n,nm,a,la,e,aa,ll,ifail,info)  
      implicit REAL*8 (a-h,r-z), integer (i-q)
      dimension a(*),la(0:*),e(*),aa(*),ll(*),info(*)
      common/noutc/nout
      common/iprintc/iprint
      common/sparsec/ns,ns1,nt,nt1,nu,nu1,nx,nx1,np,np1,nprof,
     *  lc,lc1,li,li1,lm,lm1,lp,lp1,lq,lq1,lr,lr1,ls,ls1,lt,lt1
      common/factorc/m1,m2,mp,mq,lastr,irow
      common/mxm1c/mxm1
      common/refactorc/nup,nfreq
      common/epsc/eps,tol,emin
c     write(nout,*)'pivot: p,q =',p,q
      if(p.ne.mp)then
        call eptsol(n,a,la,p,a,aa(ns1),aa(nt1),aa,aa(np1),
     *    ll,ll(lc1),ll(li1),ll(lp1),ll(lq1))
        if(p.gt.n)then
          e(p)=xlen(0.d0,aa(ns1+m2),m1)
        else
          e(p)=xlen(1.d0,aa(ns1+m2),m1)
        endif
        epp=e(p)
        mp=p
      endif
      if(q.ne.mq)then
        call aqsol(n,a,la,q,a,aa(nt1),aa(nx1),aa,aa(np1),
     *    ll,ll(lc1),ll(li1),ll(lp1),ll(lq1))
        mq=q
      endif
c  update steepest edge coefficients
      tp=aa(nt+ll(li+p))
      if(tp.eq.0.d0)tp=eps
      ep=e(p)
c     eq=ep/tp
      eq=abs(ep/tp)
      tp=tp/ep
      tpsq=tp**2
      do i=1,m2-1
        aa(nu+i)=0.d0
      enddo
      do i=m2,n
        aa(nu+i)=aa(ns+i)/ep
      enddo
      call aqsol(n,a,la,-1,a,aa(nu1),aa(nx1),aa,aa(np1),
     *  ll,ll(lc1),ll(li1),ll(lp1),ll(lq1))
c     write(nout,*)'row perm',(ll(ij),ij=1,n)
c     write(nout,*)'column perm',(ll(lc+ij),ij=1,m1)
c     write(nout,*)'s =',(aa(ns+ij),ij=1,n)
c     write(nout,*)'t =',(aa(nt+ij),ij=1,n)
c     write(nout,*)'u =',(aa(nu+ij),ij=1,n)
      e(p)=0.d0
      do i=1,nm
        if(e(i).gt.0.d0)then
          j=ll(li+i)
          ei=e(i)
c         ti=aa(nt+j)*eq/ei
c         e(i)=max(emin,ei*sqrt(max(1.E0-ti*(2.E0*aa(nu+j)/ei-ti),0.E0)))
          ti=aa(nt+j)/ei
          e(i)=max(emin,
     *      ei*sqrt(max(tpsq-ti*(2.d0*tp*aa(nu+j)/ei-ti),0.d0))*eq)
        endif
      enddo
      e(q)=max(emin,eq)
c     e(q)=max(emin,abs(eq))
      info(1)=info(1)+1
      if(nup.ge.nfreq)then
c     if(nup.ge.30)then
c  refactorize L
        ip=ll(li+p)
        if(p.gt.n)then
          m2=m2+1
          qq=ll(lc+m2)
          ll(lc+ip)=qq
          ll(li+qq)=ip
          ll(li+p)=0
        else
          ll(ip)=ll(m2)
          ll(li+ll(ip))=ip
          ll(m2)=p
          ll(li+p)=m2
        endif
        if(q.gt.n)then
          ll(lc+m2)=q
          ll(li+q)=m2
          m2=m2-1
        else
          iq=ll(li+q)
          ll(iq)=ll(m2)
          ll(li+ll(iq))=iq
          ll(m2)=q
          ll(li+q)=m2
        endif
        m1=n-m2
        call re_order(n,nm,a,la(1),la(la(0)),ll,ll(lc1),ll(li1),
     *    ll(lm1),ll(lp1),ll(lq1),ll(lr1),ll(ls1),ll(lt1),aa(np1),
     *    nprof,ifail)
        if(ifail.ge.1)then
          write(nout,*)'failure in re_order (3)'
          return
        endif
        call re_factor(n,nm,a,la,ll,ll(lc1),ll(li1),
     *    ll(lm1),ll(lp1),ll(lq1),ll(lr1),ll(ls1),ll(lt1),aa(np1),
     *    nprof,aa,ifail)
      else
c  update L
 !        write(16,*)'update'
        call update_L(p,q,n,nm,a,la,ll,ll(lc1),ll(li1),ll(lm1),ll(lp1),
     *    ll(lq1),ll(lr1),ll(ls1),aa(np1),nprof,aa,aa(ns1),ifail)
      endif
      if(ifail.eq.7)return
      mp=-1
      mq=-1
      call check_L(n,aa,ll(lp1),ifail)
c     write(nout,*)'steepest edge coefficients',(e(ij),ij=1,nm)
c     emax=0.E0
c     do i=1,nm
c       if(e(i).gt.0.E0)then
c         call eptsol(n,a,la,i,a,aa(ns1),aa(nt1),aa,aa(np1),
c    *      ll,ll(lc1),ll(li1),ll(lp1),ll(lq1))
c         ei=xlen(0.E0,aa(ns1),n)
c         ei=sqrt(scpr(0.E0,aa(ns1),aa(ns1),n))
c         emax=max(emax,abs(ei-e(i)))
c       endif
c     enddo
c     if(emax.ge.tol)
c    *  write(nout,*)'error in steepest edge coefficients =',emax
      return
      end subroutine

      subroutine fbsub(n,jmin,jmax,a,la,q,b,x,ls,aa,ll,save) 
      implicit REAL*8 (a-h,r-z), integer (i-q)
      logical save
      dimension a(*),la(*),b(*),x(*),ls(*),aa(*),ll(*)

c  solves a system  B.x=b

c  Parameter list
c  **************
c   n   number of variables (as for bqpd)
c   a,la   specification of QP problem data (as for bqpd)
c   jmin,jmax  (see description of ls below)
c   q   an integer which, if in the range 1:n+m, specifies that the rhs vector
c       b is to be column q of the matrix A of general constraint normals.
c       In this case the parameter b is not referenced by fbsub.
c       If q=0 then b is taken as the vector given in the parameter b.
c   b(n)  must be set to the r.h.s. vector b (but only if q=0)
c   x(n+m)  contains the required part of the solution x, set according to the
c       index number of that component (in the range 1:n for a simple bound and
c       n+1:n+m for a general constraint)
c   ls(*)  an index vector, listing the components of x that are required.
c       Only the absolute value of the elements of ls are used (this allows
c       the possibility of using of the contents of the ls parameter of bqpd).
c       Elements of x in the range abs(ls(j)), j=jmin:jmax are set by fbsub.
c       These contortions allow bqpd to be independent of the basis matrix code.
c   aa(*)  real storage used by the basis matrix code (supply the vector
c       ws(lu1) with ws as in the call of bqpd and lu1 as in common/bqpdc/...)
c   ll(*)  integer storage used by the basis matrix code (supply the vector
c       lws(ll1) with lws as in the call of bqpd and ll1 as in common/bqpdc/...)
c   save   indicates if fbsub is to save its copy of the solution for possible
c       future use. We suggest that the user only sets save = .false.

      common/noutc/nout
      common/sparsec/ns,ns1,nt,nt1,nu,nu1,nx,nx1,np,np1,nprof,
     *  lc,lc1,li,li1,lm,lm1,lp,lp1,lq,lq1,lr,lr1,ls_,ls1,lt,lt1
      common/factorc/m1,m2,mp,mq,lastr,irow
c     write(nout,*)'fbsub  q =',q
      if(save)then
        if(q.ne.mq)then
          call aqsol(n,a,la,q,b,aa(nt1),aa(nx1),aa,aa(np1),
     *      ll,ll(lc1),ll(li1),ll(lp1),ll(lq1))
          mq=q
        endif
        do j=jmin,jmax
          i=abs(ls(j))
          x(i)=aa(nt+ll(li+i))
        enddo
      else
!	write(6,*)
!     3	'nu1,nx1,np1,lc1,li1,lp1,lq1',nu1,nx1,np1,lc1,li1,lp1,lq1
        call aqsol(n,a,la,q,b,aa(nu1),aa(nx1),aa,aa(np1),
     *    ll,ll(lc1),ll(li1),ll(lp1),ll(lq1))
        do j=jmin,jmax
          i=abs(ls(j))
          x(i)=aa(nu+ll(li+i))
        enddo
      endif
      return
      end subroutine

      subroutine tfbsub(n,a,la,p,b,x,aa,ll,ep,save) 
      implicit REAL*8 (a-h,r-z), integer (i-q)
      logical save
      dimension a(*),la(*),b(*),x(*),aa(*),ll(*)

c  solves a system  Bt.x=b

c  Parameter list
c  **************
c   n   number of variables (as for bqpd)
c   a,la   specification of QP problem data (as for bqpd)
c   p    an integer which, if in the range 1:n+m, specifies that the rhs vector
c        b is a unit vector appropriate to the position of p in the current
c        ordering. In this case b is not referenced by tfbsub.
c   b(n+m)  If p=0, this must be set to the r.h.s. vector b. Only the components
c        of b need be set, according to the index number of each component (in
c        the range 1:n for a simple bound and n+1:n+m for a general constraint)
c   x(n)  contains the solution x (in natural ordering)
c   aa(*)  real storage used by the basis matrix code (supply the vector
c       ws(lu1) with ws as in the call of bqpd and lu1 as in common/bqpdc/...)
c   ll(*)  integer storage used by the basis matrix code (supply the vector
c       lws(ll1) with lws as in the call of bqpd and ll1 as in common/bqpdc/...)
c   ep  if p.ne.0 and save is true, ep contains the l_2 length of x on exit
c   save  indicates if tfbsub is to save its copy of the solution for possible
c       future use. We suggest that the user only sets save = .false.

      common/noutc/nout
      common/sparsec/ns,ns1,nt,nt1,nu,nu1,nx,nx1,np,np1,nprof,
     *  lc,lc1,li,li1,lm,lm1,lp,lp1,lq,lq1,lr,lr1,ls,ls1,lt,lt1
      common/factorc/m1,m2,mp,mq,lastr,irow
c     write(nout,*)'tfbsub  p =',p
      if(save)then
        if(p.ne.mp)then
          call eptsol(n,a,la,p,b,aa(ns1),aa(nt1),aa,aa(np1),
     *      ll,ll(lc1),ll(li1),ll(lp1),ll(lq1))
          mp=p
        endif
        do i=1,n
          x(ll(i))=aa(ns+i)
        enddo
        if(p.gt.n)then
          ep=xlen(0.d0,aa(ns1+m2),m1)
        elseif(p.gt.0)then
          ep=xlen(1.d0,aa(ns1+m2),m1)
        endif
      else
        call eptsol(n,a,la,p,b,aa(nu1),aa(nt1),aa,aa(np1),
     *    ll,ll(lc1),ll(li1),ll(lp1),ll(lq1))
        do i=1,n
          x(ll(i))=aa(nu+i)
        enddo
      endif
c     write(nout,*)'x =',(x(i),i=1,n)
      return
      end subroutine

      subroutine newg 
      common/factorc/m1,m2,mp,mq,lastr,irow
      mq=-1
      return
      end subroutine

c******** The following routines are internal to sparseL.f **************

      subroutine check_L(n,d,p,ifail) 
      implicit REAL*8 (a-h,r-z), integer (i-q)
      dimension d(*),p(*)
      common/noutc/nout
      common/factorc/m1,nu,mp,mq,lastr,irow
      common/epsc/eps,tol,emin
c     write(nout,*)'check_L'
      ifail=1
c     dmin=1.E37
      do k=nu+1,n
c       dmin=min(dmin,abs(d(k)))
        if(abs(d(k)).le.tol)return
      enddo
c     write(nout,*)'dmin =',dmin
c     len=0
c     do i=1,n
c       len=len+p(i)
c     enddo
c     write(nout,*)m1*(m1+1)/2,len+m1
c     write(nout,*)'m1 =',m1,'   file length =',len,'   total =',len+m1
      ifail=0
      return
      end subroutine

      subroutine aqsol(n,a,la,q,b,tn,xn,d,ws,lr,lc,li,pp,qq) 
      implicit REAL*8 (a-h,r-z), integer (i-q)
      dimension a(*),la(*),b(*),tn(*),xn(*),d(*),ws(*),
     *  lr(*),lc(*),li(*),pp(*),qq(*)
      common/noutc/nout
      common/factorc/m1,m2,mp,mq,lastr,irow
c     write(nout,*)'aqsol  q =',q
      if(q.gt.0)then
        do i=1,n
          tn(i)=0.d0
        enddo
        if(q.le.n)then
          tn(li(q))=1.d0
        else
          call iscatter(a,la,q-n,li,tn,n)
        endif
      elseif(q.eq.0)then
        do i=1,n
          tn(li(i))=b(i)
        enddo
      endif
c     write(nout,*)'tn =',(tn(i),i=1,n)
      do i=n,m2+1,-1
        ir=lr(i)
        pri=pp(ir)
        if(pri.eq.0)then
          xn(i)=tn(i)/d(i)
        else
          xn(i)=(scpr(tn(i),ws(qq(ir)+1),tn(i-pri),pri))/d(i)
        endif
        call isaipy(-xn(i),a,la,lc(i)-n,tn,n,lr,li)
      enddo
      do i=m2+1,n
        tn(i)=xn(i)
      enddo
c     write(nout,*)'tn =',(tn(i),i=1,n)
      return
      end subroutine

      subroutine eptsol(n,a,la,p,b,sn,tn,d,ws,lr,lc,li,pp,qq) 
      implicit REAL*8 (a-h,r-z), integer (i-q)
      dimension a(*),la(*),b(*),sn(*),tn(*),d(*),ws(*),
     *  lr(*),lc(*),li(*),pp(*),qq(*)
      common/noutc/nout
      common/iprintc/iprint
      common/epsc/eps,tol,emin
      common/factorc/m1,m2,mp,mq,lastr,irow
c     write(nout,*)'eptsol  p =',p
      if(p.eq.0)then
        do i=1,m2
          sn(i)=b(lr(i))
        enddo
        do i=m2+1,n
          sn(i)=0.d0
        enddo
        do i=m2+1,n
          j=lc(i)
          sn(i)=-aiscpri(n,a,la,j-n,sn,-b(j),lr,li)/d(i)
          ir=lr(i)
          pri=pp(ir)
          if(pri.gt.0)call mysaxpy(sn(i),ws(qq(ir)+1),sn(i-pri),pri)
        enddo
      else
        do i=1,n
          sn(i)=0.d0
        enddo
        pr=li(p)
        if(p.le.n)then
          if(pr.gt.m2)goto1
          sn(pr)=1.d0
          do i=m2+1,n
            sn(i)=-aiscpri(n,a,la,lc(i)-n,sn,0.d0,lr,li)/d(i)
            ir=lr(i)
            pri=pp(ir)
            if(pri.gt.0)call mysaxpy(sn(i),ws(qq(ir)+1),sn(i-pri),pri)
          enddo
        else
          if(pr.le.m2)goto1
          do i=m2+1,n
            bi=0.d0
            if(i.eq.pr)bi=-1.d0
            sn(i)=-aiscpri(n,a,la,lc(i)-n,sn,bi,lr,li)/d(i)
            ir=lr(i)
            pri=pp(ir)
            if(pri.gt.0)call mysaxpy(sn(i),ws(qq(ir)+1),sn(i-pri),pri)       
          enddo
        endif
      endif
c     write(nout,*)'sn =',(sn(i),i=1,n)
      return
    1 continue
      write(nout,*)'malfunction detected in eptsol: p =',p
      stop
      end subroutine

      subroutine order(n,nu,nc,la,lr,ls,li,p,q,r,ws,mxws,ifail) 
	use lastmod
      implicit integer (c-t)
      REAL*8 ws
      dimension la(0:*),lr(*),ls(*),li(*),p(*),q(*),r(*),ws(*)
      common/noutc/nout
c     character star(1000,80)
c     write(nout,*)'order'
c  spk1 ordering on full matrix
      ifail=0
      if(nu.eq.n)return
c  set row and column counts and row-wise data structure
      nn=n-nu
      ii=mxws/nn
      do j=1,nn
        rowj=lr(j)
        p(rowj)=(j-1)*ii
        r(rowj)=0
      enddo
      do j=nn+1,n
        r(lr(j))=0
      enddo
    1 continue
      do i=nu+1,nc
        coli=abs(ls(i))
        li(coli)=0
        jp=la(0)+coli-n
*
        do j=la(jp),last(coli-n) !la(jp+1)-1
          rowj=la(j)
          if(li(rowj).le.nn)then
            li(coli)=li(coli)+1
            r(rowj)=r(rowj)+1
            ij=p(rowj)+r(rowj)
            if(ij.gt.mxws)then
              ij=mxws
              ifail=1
            endif
            ws(ij)=Real(coli)
          endif
        enddo
      enddo
c  check for no overlaps
      qrj=0
      do j=1,nn
        rowj=lr(j)
        if(p(rowj).lt.qrj)ifail=1
        qrj=p(rowj)+r(rowj)
        q(rowj)=qrj
        p(rowj)=p(rowj)+1
      enddo
      if(ifail.eq.1.or.qrj.gt.mxws)then
        qrj=0
        do j=1,nn
          rowj=lr(j)
          p(rowj)=qrj
          qrj=qrj+r(rowj)
          r(rowj)=0
        enddo
        if(qrj.gt.mxws)then
          write(nout,*)'not enough space for ws in order:  mxws =',mxws
          ifail=7
          return
        endif
        ifail=0
        goto1
      endif
      ifirstc=nu+1
      ifirstr=1
    2 continue
c  move zero-column-count columns to lhs and find minimum column count
      mcc=n
      do i=ifirstc,nc
        coli=abs(ls(i))
        if(li(coli).eq.0)then
          call iexch(ls(i),ls(ifirstc))
          li(coli)=ifirstr-1
          ifirstc=ifirstc+1
        else
          mcc=min(mcc,li(coli))
        endif
      enddo
c     write(nout,*)'ifirstc,ifirstr,mcc',ifirstc,ifirstr,mcc
c     write(nout,*)'lr =',(lr(j),j=1,n)
c     write(nout,*)'ls =',(ls(i),i=nu+1,nc)
c     write(nout,*)'row counts =',(r(lr(j)),j=1,n)
c     write(nout,*)'column counts =',(li(abs(ls(i))),i=nu+1,nc)
      if(ifirstc.gt.nc)goto4
c  apply tie-break rule
      tie=0
      do i=ifirstc,nc
        coli=abs(ls(i))
        if(li(coli).eq.mcc)then
          ti=0
          jp=la(0)+coli-n
*jsp
          do j=la(jp),last(coli-n) !la(jp+1)-1
            rowj=la(j)
            if(li(rowj).ge.ifirstr)ti=ti+r(rowj)
          enddo
          if(ti.gt.tie)then
            tie=ti
            mccc=coli
          endif
        endif
      enddo
c     write(nout,*)'tie,mccc',tie,mccc
c  permute rows of m-c-c column to top and update column counts
      jp=la(0)+mccc-n
*jsp
      do j=la(jp),last(mccc-n) !la(jp+1)-1
        rowj=la(j)
        jr=li(rowj)
        if(jr.lt.ifirstr)goto3
        if(jr.gt.nn)goto3
        lr(jr)=lr(ifirstr)
        li(lr(jr))=jr
        lr(ifirstr)=rowj
        li(rowj)=ifirstr
        ifirstr=ifirstr+1
        do i=p(rowj),q(rowj)
          coli=int(ws(i))
          li(coli)=li(coli)-1
        enddo
    3   continue
      enddo
      goto2
    4 continue
c  print star diagram
c     if(nc-nu.gt.80.or.n.gt.1000)stop
c     write(nout,*)'spk1 ordering'
c     ij=li(abs(ls(nc)))
c     do i=1,ij
c       do j=1,nc-nu
c         star(i,j)=' '
c       enddo
c     enddo
c     do j=1,nc-nu
c       jp=la(0)+abs(ls(nu+j))-n
c       do i=la(jp),la(jp+1)-1
c         star(li(la(i)),j)='*'
c       enddo
c     enddo
c     do i=1,ij
c       write(nout,*)(star(i,j),j=1,nc-nu)
c     enddo
c     write(nout,*)'lr =',(lr(i),i=1,n)
c     write(nout,*)'ls =',(ls(i),i=nu+1,nc)
c     write(nout,*)'lower profile =',(li(abs(ls(i))),i=nu+1,nc)
      return
      end subroutine

      subroutine factor(n,nm,nu,nc,a,la,e,ls,sn,tn,un,xn,lr,lc,li, 
     *  mao,p,q,r,s,ws,mxws,d,ifail)
      implicit REAL*8 (a-h,r-z), integer (i-q)
      integer coli,r,s,rowi,rowp,tl,tu
      dimension a(*),la(0:*),e(*),ls(*),sn(*),tn(*),un(*),xn(*),
     *  lr(*),lc(*),li(*),mao(*),p(*),q(*),r(*),s(*),ws(*),d(*)
c     character star(1000,80)
      common/factorc/m1,m2,mp,mq,lastr,irow
      common/iprintc/iprint
      common/refactorc/nup,nfreq
      common/epsc/eps,tol,emin
      common/noutc/nout
      parameter (thresh=1.d-1)
c  factorize LPA=U when A is rectangular
c    p(row) stores the number of stored elements of a natural row
c    q(row) stores the base address in ws of a natural row
c    r(row) stores the previous row stored in ws (or 0 if the first row in ws)
c    s(row) stores the next row stored in ws (or 0 if the last row in ws)
c    li(n+*) stores the lower profile of the sparse matrix
c    irow stores the natural row number of the initial row stored in ws
c    lastr stores the natural row number of the previous row put into ws
c     write(nout,*)'factor'
      nup=0
      lastr=0
      irow=0
      do i=1,n
        p(i)=0
      enddo
      m1=0
      tl=1
      do ii=nu+1,nc
        coli=abs(ls(ii))
c       write(nout,*)'coli =',coli
        tu=li(coli)
        do i=1,n
          tn(i)=0.d0
        enddo
        call iscatter(a,la,coli-n,li,tn,n)
        do i=m1,1,-1
          rowi=lr(i)
          pri=p(rowi)
          if(pri.eq.0)then
            xn(i)=tn(i)/d(i)
          else
            xn(i)=(scpr(tn(i),ws(q(rowi)+1),tn(i-pri),pri))/d(i)
          endif
          call isaipy(-xn(i),a,la,lc(i)-n,tn,n,lr,li)
        enddo
        do i=1,m1
          tn(i)=xn(i)
        enddo
        m1p=m1+1
c       write(nout,*)'lr =',(lr(i),i=1,n)
c       write(nout,*)'tn =',(tn(i),i=1,tu)
c  threshold pivot selection
        call linf(tu-m1,tn(m1p),z,iz)
        if(z.le.tol)then
          li(coli)=0
          goto2
        endif
        zz=max(tol,z*thresh)
        do i=tl,tu
          q(lr(i))=m1p
        enddo
c       write(nout,*)'q =',(q(lr(i)),i=m1p,tu)
        iz=iz+m1
        if(iz.lt.tl)then
          z=0.d0
          qri=m1p
          do j=m1p,tu
            tnj=abs(tn(j))
            if(tnj.ge.zz)then
              qrj=q(lr(j))
              if(qrj.eq.qri)then
                if(tnj.gt.z)then
                  z=tnj
                  iz=j
                endif
              elseif(qrj.gt.qri)then
                z=tnj
                iz=j
                qri=qrj
              endif
            endif
          enddo
        endif
        tl=tu+1
c       write(nout,*)'zz,z,iz,m1,qri',zz,z,iz,m1,qri
        if(iz.gt.m1p)then
          call rexch(tn(m1p),tn(iz))
          call iexch(lr(m1p),lr(iz))
          li(lr(m1p))=m1p
          li(lr(iz))=iz
        endif
        rowp=lr(m1p)
c  reset q values
        qrp=q(rowp)
        do i=m1p+1,tu
          if(abs(tn(i)).gt.tol)then
            rowi=lr(i)
            if(qrp.lt.q(rowi))q(rowi)=qrp
          endif
        enddo
        tnp=tn(m1p)
        do i=1,n
          sn(i)=0.d0
        enddo
        sn(m1p)=1.d0
        do i=1,m1
          sn(i)=-aiscpri(n,a,la,lc(i)-n,sn,0.d0,lr,li)/d(i)
          rowi=lr(i)
          pri=p(rowi)
          if(pri.gt.0)call mysaxpy(sn(i),ws(q(rowi)+1),sn(i-pri),pri)
        enddo
c       write(nout,*)'sn =',(sn(i),i=1,m1)
        ep=e(rowp)
        eq=abs(ep/tnp)
        tp=tnp/ep
        tpsq=tp**2
        e(rowp)=0.d0
c  update steepest edge coefficients
        do i=1,m1p
          un(i)=sn(i)/ep
        enddo
        do i=m1p+1,n
          un(i)=0.d0
        enddo
        do i=m1,1,-1
          rowi=lr(i)
          pri=p(rowi)
          if(pri.eq.0)then
            xn(i)=un(i)/d(i)
          else
            xn(i)=(scpr(un(i),ws(q(rowi)+1),un(i-pri),pri))/d(i)
          endif
          call isaipy(-xn(i),a,la,lc(i)-n,un,n,lr,li)
        enddo
        do i=1,m1
          un(i)=xn(i)
        enddo
c       write(nout,*)'un =',(un(i),i=1,n)
        do i=1,nm
          if(e(i).gt.0.d0)then
            j=li(i)
            ei=e(i)
            tni=tn(j)/ei
            e(i)=max(emin,
     *        ei*sqrt(max(tpsq-tni*(2.d0*tp*un(j)/ei-tni),0.d0))*eq)
          endif
        enddo
        e(coli)=max(emin,eq)
        do j=qrp,m1
          if(abs(sn(j)).gt.tol)goto1
        enddo
        j=m1p
    1   continue
        pri=m1p-j
        if(pri.gt.0)then
          call newslot(rowp,pri,lastr,irow,p,q,r,s,ws,mxws,i,ifail)
          if(ifail.gt.0)return
          p(rowp)=pri
          i=q(rowp)
          do j=j,m1
            i=i+1
            ws(i)=sn(j)
          enddo
        endif
        m1=m1p
        ls(m1)=ls(ii)
        lc(m1)=coli
        li(coli)=m1
        d(m1)=tnp
    2   continue
      enddo
c  complete ls and reorder lr, lc and d
      do i=m1+1,n
        ls(i)=lr(i)
      enddo
      j=n
      do i=1,nm
        if(e(i).eq.0.d0)then
          j=j+1
          ls(j)=i
        endif
      enddo
      m2=n-m1
      do i=n,m2+1,-1
        lc(i)=lc(i-m2)
        li(lc(i))=i
        lr(i)=lr(i-m2)
        li(lr(i))=i
        d(i)=d(i-m2)
      enddo
      do i=1,m2
        lr(i)=ls(m1+i)
        li(lr(i))=i
      enddo
c  reset mao
      ilast=n
      ii=ilast
      do i=ilast,m2+1,-1
        mao(i)=ilast
        ii=min(ii,i-p(lr(i)))
        if(ii.eq.i)ilast=i-1
      enddo
c     write(nout,*)'PAQ factors:  m1 =',m1
c     write(nout,*)'d =',(d(ij),ij=m2+1,n)
c     do j=m2+1,n
c       rowp=lr(j)
c       if(p(rowp).ne.0)then
c         write(nout,*)'L(',rowp,')',
c    *      (ws(k),k=q(rowp)+1,q(rowp)+p(rowp))
c       endif
c     enddo
c  print star diagram
c     write(nout,*)'factored ordering:  m1 =',m1
c     if(m1.gt.80.or.n.gt.1000)stop
c     do i=1,n
c       do j=1,m1
c         star(i,j)=' '
c       enddo
c     enddo
c     do j=1,m1
c       jp=la(0)+lc(m2+j)-n
c       do i=la(jp),la(jp+1)-1
c         star(li(la(i)),j)='*'
c       enddo
c     enddo
c     do i=m2+1,n
c       write(nout,*)(star(i,j),j=1,m1)
c     enddo
c     write(nout,*)'ls =',(ls(j),j=1,n)
c     write(nout,*)'s.e. coeffs =',(e(i),i=1,nm)
c     write(nout,*)'lr =',(lr(j),j=1,n)
c     write(nout,*)'lc =',(lc(j),j=m2+1,n)
c     write(nout,*)'mao =',(mao(j),j=m2+1,n)
c     call checkout(n,a,la,lr,lc,li,p,q,r,s,ws,mxws,d)
      return
      end subroutine

      subroutine re_order(n,nm,a,la,point,lr,lc,li,mao,p,q,r,s, 
     *  t,ws,mxws,ifail)
	 use lastmod
      implicit REAL*8 (a-h,u-z), integer (i-t)
      dimension a(*),la(*),point(0:*),lr(*),lc(*),li(*),mao(*),
     *  p(*),q(*),r(*),s(*),t(*),ws(*)
      common/factorc/m1,nu,mp,mq,lastr,irow
      common/noutc/nout
      logical backtrack
c     character star(1000,80)
c  print star diagram
c     if(n-nu.gt.80.or.n.gt.1000)stop
c     write(nout,*)'initial ordering'
c     do i=1,n
c       do j=1,n-nu
c         star(i,j)=' '
c       enddo
c     enddo
c     do j=1,n-nu
c       ilp=lc(nu+j)-n
c       do i=point(ilp),point(ilp+1)-1
c         star(li(la(i)),j)='*'
c       enddo
c     enddo
c     do i=nu+1,n
c       write(nout,*)(star(i,j),j=1,n-nu)
c     enddo
c     write(nout,*)'re_order'
      if(nu.eq.n)then
        ifail=0
        return
      endif
      m=nm-n
c  transversal search
      do iq=nu+1,n
        backtrack=.false.
        istack=nu
        inode=iq
        nodec=lc(inode)
        nodec_n=nodec-n
!	call re_order(n,nm,a,la(1),la(la(0)),ll,ll(lc1),ll(li1)
*                                 point  =>point(i) start of col i
*jsp
        lap=last(nodec_n)+1-point(nodec_n) ! point(nodec_n+1)-point(nodec_n)
c       write(nout,*)'column node =',nodec,'  look-ahead rows =',
c    *    (la(j),j=point(nodec_n),point(nodec_n)+lap-1)
c  look-ahead loop
    1   continue
          lap=lap-1
          nextr=la(point(nodec_n)+lap)
          inext=li(nextr)
          if(inext.ge.iq)goto4
          if(lap.gt.0)goto1
          li(nodec)=0
    2   continue
c  reassignment depth first search
*jsp
        t(inode)=last(nodec_n)+1 -point(nodec_n) ! point(nodec_n+1)-point(nodec_n)
c       write(nout,*)'column node =',nodec,'  unfathomed rows =',
c    *    (la(j),j=point(nodec_n),point(nodec_n)+t(inode)-1)
    3   continue
c  examine successor nodes
        if(t(inode).eq.0)then
          if(istack.eq.nu)then
            ifail=1
c           ifail=iq
c           write(nout,*)'exit: ifail =',iq
            return
          endif
          istack=istack-1
          backtrack=.true.
          if(istack.eq.nu)then
            inode=iq
          else
            inode=mao(istack)
          endif
c         write(nout,*)'backtrack to node at address =',inode
          nodec=lc(inode)
          nodec_n=nodec-n
c         write(nout,*)'column node =',nodec,'  unfathomed rows =',
c    *      (la(j),j=point(nodec_n),point(nodec_n)+t(inode)-1)
          goto3
        endif
        t(inode)=t(inode)-1
        nextr=la(point(nodec_n)+t(inode))
        inext=li(nextr)
        if(inext.le.nu)goto3
        if(t(inext).ge.0)goto3
c  extend depth first search
c       write(nout,*)'nextr,inext',nextr,inext
        inode=inext
c       write(nout,*)'put node address on stack'
        istack=istack+1
        mao(istack)=inode
c       write(nout,*)'stack =',(mao(j),j=nu+1,istack)
        nodec=lc(inode)
        nodec_n=nodec-n
        lap=li(nodec)
        if(lap.eq.0)goto2
c       write(nout,*)'column node =',nodec,'  look-ahead rows =',
c    *    (la(j),j=point(nodec_n),point(nodec_n)+lap-1)
        goto1
    4   continue
c       write(nout,*)'new assignment found in row',nextr
c       write(nout,*)'istack,inext,nextr',istack,inext,nextr
c       if(istack.gt.nu)write(nout,*)'stack =',(mao(j),j=nu+1,istack)
        li(nodec)=lap
c  perform row permutation
        lr(inext)=lr(iq)
        li(lr(inext))=inext
        inode=iq
        do i=nu+1,istack
          inext=mao(i)
          lr(inode)=lr(inext)
          li(lr(inode))=inode
          inode=inext
        enddo
        lr(inode)=nextr
        li(nextr)=inode
c       write(nout,*)'lr =',(lr(j),j=nu+1,n)
c       write(nout,*)'look-ahead lengths =',(li(lc(j)),j=nu+1,iq)
        t(iq)=-1
        if(backtrack.or.istack.gt.nu+1)then
          do i=nu+1,iq-1
            t(i)=-1
          enddo
        endif
        do i=1,n
          if(li(i).gt.n)then
            write(nout,*)'iq =',iq
            stop
          endif
        enddo
      enddo
c     write(nout,*)'transversal found'
c     write(nout,*)'lr =',(lr(j),j=1,n)
c     write(nout,*)'lc =',(lc(j),j=nu+1,n)
c  print star diagram
c     if(n-nu.gt.80.or.n.gt.1000)stop
c     write(nout,*)'transversal ordering'
c     do i=1,n
c       do j=1,n-nu
c         star(i,j)=' '
c       enddo
c     enddo
c     do j=1,n-nu
c       ilp=lc(nu+j)-n
c       do i=point(ilp),point(ilp+1)-1
c         star(li(la(i)),j)='*'
c       enddo
c     enddo
c     do i=nu+1,n
c       write(nout,*)(star(i,j),j=1,n-nu)
c     enddo

c  tarjan ordering
      do i=1,n
        q(i)=0
        r(i)=0
      enddo
c  reset li and pair off columns with rows
      do i=nu+1,n
        nodec=lc(i)
        li(nodec)=i
        t(lr(i))=nodec
        s(i)=0
      enddo
      do i=nu+1,n
        noder=lr(i)
        nodec=t(noder)
*jsp
        lc(noder)=last(nodec-n)+1-point(nodec-n) ! point(nodec-n+1)-point(nodec-n)
        li(nodec)=-1
      enddo
      ifath=nu
      istack=n+1
c  tarjan loop
   10 continue
        istack=istack-1
        inode=istack
        noder=lr(inode)
        if(lc(noder).eq.0)then
          write(nout,*)'malfunction: zero length'
          stop
        endif
        nodec=t(noder)
   11   continue
        li(nodec)=lc(noder)
        mao(inode)=istack
c       write(nout,*)'put new node',noder,' on stack'
c       write(nout,*)'active part of lr =',(lr(j),j=ifath+1,n)
c       write(nout,*)'ifath,istack =',ifath,istack
c       write(nout,*)'column node =',nodec,'  unfathomed rows =',
c    *    (la(j),j=point(nodec-n),point(nodec-n)+li(nodec)-1)
   12   continue
          if(li(nodec).eq.0)then
c           write(nout,*)'backtrack to previous nodes'
   13       continue
              if(inode.eq.n)goto14
              inext=inode+1
              nextr=lr(inext)
              if(mao(inode).lt.mao(inext))goto14
              inode=inext
              noder=nextr
              nodec=t(noder)
              if(li(nodec).eq.0)goto13
c           write(nout,*)'stack =',(lr(j),j=istack,n)
c           write(nout,*)'lengths =',(li(t(lr(j))),j=istack,n)
c           write(nout,*)'column node =',nodec,'  unfathomed rows =',
c    *        (la(j),j=point(nodec-n),point(nodec-n)+li(nodec)-1)
            goto12
          endif
c  examine successors of current node
          li(nodec)=li(nodec)-1
          nextr=la(point(nodec-n)+li(nodec))
          inext=li(nextr)
          if(inext.le.ifath)goto12
          q(nextr)=q(nextr)+1
          nextc=t(nextr)
c         write(nout,*)'nextc,nextr,inext',nextc,nextr,inext
          if(li(nextc).ge.0)then
            mx=mao(inext)
            if(mao(inode).ge.mx)goto12
            do j=istack,n
              if(mao(j).eq.mx)goto12
              mao(j)=mx
            enddo
            write(nout,*)'malfunction'
            stop
          endif
          nodec=nextc
          noder=nextr
          istack=istack-1
          inode=istack
          lr(inext)=lr(inode)
          li(lr(inext))=inext
          lr(inode)=noder
          li(noder)=inode
          goto11
   14   continue
c       write(nout,*)'strong component identified'
c       write(nout,*)'active part of lr =',(lr(j),j=ifath+1,n)
c       write(nout,*)'ifath,istack,inode =',ifath,istack,inode,n
c  shift forward strong component
        inext=istack-1
        ir=inode-inext
        do j=istack,inode
          mao(j)=lr(j)
        enddo
        do j=inext+ir,ifath+1+ir,-1
          lr(j)=lr(j-ir)
          li(lr(j))=j
        enddo
        mx=ifath+ir
        iq=inext-ifath
        ifath=ifath+1
        do j=ifath,mx
          lr(j)=mao(j+iq)
          li(lr(j))=j
          mao(j)=mx
        enddo
        istack=inode+1
        ifath=mx
c       write(nout,*)'active part of lr =',(lr(j),j=ifath+1,n)
c       write(nout,*)'ifath,istack =',ifath,istack
        if(istack.le.n)then
          inode=istack
          noder=lr(inode)
          nodec=t(noder)
          nodec_n=nodec-n
c         write(nout,*)'column node =',nodec,'  unfathomed rows =',
c    *      (la(j),j=point(nodec-n),point(nodec-n)+li(nodec)-1)
          goto12
        endif
      if(ifath.lt.n)goto10
c  end of tarjan process
c  reset lc and li
      do i=nu+1,n
        lc(i)=t(lr(i))
        li(lc(i))=i
      enddo
c     write(nout,*)'mao =',(mao(j),j=nu+1,n)
c     write(nout,*)'q =',(q(j),j=1,n)
c     write(nout,*)'lr =',(lr(j),j=1,n)
c     write(nout,*)'lc =',(lc(j),j=nu+1,n)
c     write(nout,*)'li =',(li(j),j=1,n+m)
c  print star diagram
c     if(n-nu.gt.80.or.n.gt.1000)stop
c     write(nout,*)'tarjan ordering'
c     do i=1,n
c       do j=1,n-nu
c         star(i,j)=' '
c       enddo
c     enddo
c     do j=1,n-nu
c       ilp=lc(nu+j)-n
c       do i=point(ilp),point(ilp+1)-1
c         star(li(la(i)),j)='*'
c       enddo
c     enddo
c     do i=nu+1,n
c       write(nout,*)(star(i,j),j=1,n-nu)
c     enddo
c  set up pointers for row-wise sparse structure
      p(1)=1
      do i=1,n-1
        p(i+1)=p(i)+q(i)
        q(i)=p(i)-1
      enddo
      if(p(n)+q(n).gt.mxws)then
        ifail=7
        return
      endif
      q(n)=p(n)-1
      i=nu+1
   20 continue
      if(i.eq.mao(i))then
        t(i)=i
      else
c  spk1 ordering on tarjan block
c  set row and column counts
        do inode=i,mao(i)
          nodec=lc(inode)
*jsp
          do j=point(nodec-n),last(nodec-n) ! (point(nodec-n+1)-1
            noder=la(j)
            if(li(noder).ge.i)then
              q(noder)=q(noder)+1
              ws(q(noder))=dble(nodec) ! Real(nodec)
              s(inode)=s(inode)+1
            endif
          enddo
        enddo
c  find minimum-column-count column
        mcc=n
        do inode=i,mao(i)
          noder=lr(inode)
          r(noder)=q(noder)-p(noder)+1
          mcc=min(mcc,s(inode))
        enddo
c     write(nout,*)'i,mao(i),mcc',i,mao(i),mcc
c     write(nout,*)'p =',(p(lr(j)),j=i,mao(i))
c     write(nout,*)'q =',(q(lr(j)),j=i,mao(i))
c     write(nout,*)'r =',(r(lr(j)),j=i,mao(i))
c     write(nout,*)'s =',(s(j),j=i,mao(i))
c  check for fully dense block
        if(mcc.gt.mao(i)-i)then
          do inode=i,mao(i)
            t(inode)=mao(i)
          enddo
          goto22
        endif
c  determine spk1 ordering
        ifirstr=i
        ifirstc=i
   21   continue
c  apply tie-break rule
        tie=0
        do inode=ifirstc,mao(i)
          if(s(inode).eq.mcc)then
            nodec=lc(inode)-n
            ti=0
*jsp
            do j=point(nodec),last(nodec) !point(nodec+1)-1
              noder=la(j)
              if(li(noder).ge.ifirstr)ti=ti+r(noder)
            enddo
            if(ti.gt.tie)then
              tie=ti
              mccc=nodec
            endif
          endif
        enddo
c       write(nout,*)'tie,mccc',tie,mccc+n
c  permute rows of m-c-c column to top and update column counts
*jsp
        do j=point(mccc),last(mccc) ! point(mccc+1)-1
          noder=la(j)
          ir=li(noder)
          if(ir.ge.ifirstr)then
            lr(ir)=lr(ifirstr)
            li(lr(ir))=ir
            lr(ifirstr)=noder
            li(noder)=ifirstr
            ifirstr=ifirstr+1
            do ir=p(noder),q(noder)
              inode=li(int(ws(ir)))
              s(inode)=s(inode)-1
            enddo
          endif
        enddo
c       write(nout,*)'s =',(s(ij),ij=i,mao(i))
c       write(nout,*)'lr =',(lr(ij),ij=i,mao(i))
c  move zero-column-count columns to lhs and find minimum column count
        mcc=n
        do inode=ifirstc,mao(i)
          if(s(inode).eq.0)then
            nodec=lc(inode)
            lc(inode)=lc(ifirstc)
            li(lc(inode))=inode
            lc(ifirstc)=nodec
            li(nodec)=ifirstc
            s(inode)=s(ifirstc)
            t(ifirstc)=ifirstr-1
            ifirstc=ifirstc+1
          else
            mcc=min(mcc,s(inode))
          endif
        enddo
c       write(nout,*)'lc =',(lc(ij),ij=i,mao(i))
c       write(nout,*)'ifirstc,mcc',ifirstc,mcc
        if(ifirstc.lt.mao(i))goto21
      endif
   22 continue
      i=mao(i)+1
      if(i.le.n)goto20
c  print star diagram
c     if(n-nu.gt.80.or.n.gt.1000)stop
c     write(nout,*)'tarjan + spk1 ordering'
c     do i=1,n
c       do j=1,n-nu
c         star(i,j)=' '
c       enddo
c     enddo
c     do j=1,n-nu
c       ilp=lc(nu+j)-n
c       do i=point(ilp),point(ilp+1)-1
c         star(li(la(i)),j)='*'
c       enddo
c     enddo
c     do i=nu+1,n
c       write(nout,*)(star(i,j),j=1,n-nu)
c     enddo
c     write(nout,*)'lr =',(lr(j),j=nu+1,n)
c     write(nout,*)'lc =',(lc(j),j=nu+1,n)
c     write(nout,*)'lower profile =',(t(j),j=nu+1,n)
      ifail=0
      return
      end subroutine

      subroutine re_factor(n,nm,a,la,lr,lc,li,mao,p,q,r,s, 
     *  t,ws,mxws,d,ifail)
      implicit REAL*8 (a-h,u-z), integer (i-t)
      dimension a(*),la(0:*),lr(*),lc(*),li(*),mao(*),
     *  p(*),q(*),r(*),s(*),t(*),d(*),ws(*)
c     character star(1000,80)
      common/factorc/m1,nu,mp,mq,lastr,irow
      common/iprintc/iprint
      common/refactorc/nup,nfreq
      common/epsc/eps,tol,emin
      common/noutc/nout
      REAL*8 thresh,tol
      parameter (thresh=1.d-1)
c  factorize LPA=U
c    p(row) stores the number of stored elements of a natural row
c    q(row) stores the base address in ws of a natural row
c    r(row) stores the previous row stored in ws (or 0 if the first row in ws)
c    s(row) stores the next row stored in ws (or 0 if the last row in ws)
c    t(*) stores the lower profile of the sparse matrix
c    irow stores the natural row number of the initial row stored in ws
c    lastr stores the natural row number of the previous row put into ws
c     write(nout,*)'re_factor'
      nup=0
      m=nm-n
      lastr=0
      irow=0
      do i=1,n
        p(i)=0
      enddo
      if(m1.eq.0)return
      i=nu+1
    1 continue
      if(i.eq.mao(i))then
        d(i)=aij(lr(i),lc(i)-n,a,la)
        if(d(i).eq.0.d0)d(i)=eps
c       write(nout,*)'row,col,d(i) =',lr(i),lc(i),d(i)
      else
c       write(nout,*)'lc =',(lc(j),j=i,mao(i))
        do inode=i,mao(i)-1
          nodec=lc(inode)-n
          im=inode-1
c  form L.a_q
          z=0.
c         write(nout,*)'inode,t(inode)',inode,t(inode)
          do j=inode,t(inode)
            rowj=lr(j)
            prj=p(rowj)
            if(prj.gt.0)then
              d(j)=aiscpri2(n,a,la,rowj,nodec,ws(q(rowj)+1),1.d0,im,
     *          prj,li)
            else
              d(j)=aij(rowj,nodec,a,la)
            endif
            z=max(z,abs(d(j)))
          enddo
c         write(nout,*)'d =',(d(ij),ij=inode,t(inode))
c  threshold pivot selection
          zz=z*thresh
          z=0.d0
          pri=n
          do j=inode,t(inode)
            dj=abs(d(j))
            if(dj.ge.zz)then
              prj=p(lr(j))
              if(prj.eq.pri)then
                if(dj.gt.z)then
                  z=dj
                  iz=j
                endif
              elseif(prj.lt.pri)then
                z=dj
                iz=j
                pri=prj
              endif
            endif
          enddo
c       write(nout,*)'zz,z,iz,pri',zz,z,iz,pri
          if(iz.gt.inode)then
c  pivot interchange
            call rexch(d(inode),d(iz))
            call iexch(lr(inode),lr(iz))
            li(lr(iz))=iz
            li(lr(inode))=inode
          endif
          if(d(inode).eq.0.d0)d(inode)=eps
c  update L
          qri=q(lr(inode))
          zz=-d(inode)
          do j=inode+1,t(inode)
            z=d(j)/zz
            rowj=lr(j)
            prj=p(rowj)
            qrj=q(rowj)
c  find space available in-situ in ws
            if(prj.eq.0)then
              len=0
            elseif(s(rowj).eq.0)then
              len=mxws-qrj
            else
              len=q(s(rowj))-qrj
            endif
            if(abs(z).le.tol)then
c  special case of a zero multiplier
              if(prj.eq.0)goto2
              len_=prj+1
              if(len_.gt.len)then
                call newslot(rowj,len_,lastr,irow,p,q,r,s,ws,mxws,qrj,
     *            ifail)
                if(ifail.gt.0)return
                qrj_=q(rowj)
                do k=1,prj
                  ws(qrj_+k)=ws(qrj+k)
                enddo
                ws(qrj_+len_)=z
              else
                ws(qrj+len_)=z
              endif
              p(rowj)=len_
              goto2
            endif
            len_=max(pri,prj)+1
            if(len_.gt.len.or.pri.gt.prj)then
c  create a new slot and use saxpyz ...
              call newslot(rowj,len_,lastr,irow,p,q,r,s,ws,mxws,qrj,
     *          ifail)
              if(ifail.gt.0)return
              qrj_=q(rowj)
              len=prj-pri
              if(len.ge.0)then
                do k=1,len
                  ws(qrj_+k)=ws(qrj+k)
                enddo
                len=len+1
                call saxpyz(z,ws(qri+1),ws(qrj+len),ws(qrj_+len),
     *            len_-len)
              else
                len=-len
                do k=1,len
                  ws(qrj_+k)=z*ws(qri+k)
                enddo
                len=len+1
                call saxpyz(z,ws(qri+len),ws(qrj+1),ws(qrj_+len),
     *            len_-len)
              endif
              ws(qrj_+len_)=z
            else
c  ... else saxpy in-situ
              if(pri.gt.0)
     *          call mysaxpy(z,ws(qri+1),ws(qrj+prj-pri+1),pri)
              ws(qrj+len_)=z
            endif
            p(rowj)=len_
c           do rj=1,n
c             if(p(rj).ne.0)then
c               write(nout,*)'storage for row',rj,'  p,q,r,s =',
c    *            p(rj),q(rj),r(rj),s(rj)
c             endif
c           enddo
    2       continue
          enddo
c         write(nout,*)'lr =',(lr(j),j=i,mao(i))
c         do j=i,mao(i)
c           rowj=lr(j)
c           if(p(rowj).ne.0)then
c             write(nout,*)'L(',rowj,')',
c    *          (ws(k),k=q(rowj)+1,q(rowj)+p(rowj))
c           endif
c         enddo
        enddo
        inode=mao(i)
        noder=lr(inode)
        pri=p(noder)
        if(pri.gt.0)then
         d(inode)=aiscpri2(n,a,la,noder,lc(inode)-n,ws(q(noder)+1),
     *     1.d0,inode-1,pri,li)
        else
          d(inode)=aij(noder,lc(inode)-n,a,la)
        endif
        if(d(inode).eq.0.d0)d(inode)=eps
      endif
      i=mao(i)+1
      if(i.le.n)goto1
c     write(nout,*)'PAQ factors:  nu =',nu
c     write(nout,*)'column perm =',(lc(j),j=nu+1,n)
c     write(nout,*)'row perm =',(lr(j),j=nu+1,n)
c     write(nout,*)'d =',(d(ij),ij=nu+1,n)
c     do j=nu+1,n
c       rowj=lr(j)
c       if(p(rowj).ne.0)then
c         write(nout,*)'L(',rowj,')',
c    *      (ws(k),k=q(rowj)+1,q(rowj)+p(rowj))
c       endif
c     enddo
c     call checkout(n,a,la,lr,lc,li,p,q,r,s,ws,mxws,d)
c  print star diagram
c     if(m1.gt.80.or.n.gt.1000)stop
c     write(nout,*)'factored tarjan + spk1 ordering:  nu =',nu
c     do i=1,n
c       do j=1,m1
c         star(i,j)=' '
c       enddo
c     enddo
c     do j=1,m1
c       jp=la(0)+lc(nu+j)-n
c       do i=la(jp),la(jp+1)-1
c         star(li(la(i)),j)='*'
c       enddo
c     enddo
c     do i=nu+1,n
c       write(nout,*)(star(i,j),j=1,m1)
c     enddo
c     write(nout,*)'lr =',(lr(j),j=nu+1,n)
c     write(nout,*)'lc =',(lc(j),j=nu+1,n)
      mp=-1
      mq=-1
      ifail=0
      return
      end subroutine

      function aiscpri2(n,a,la,rowi,coli,ws,di,im,pri,li) 
	use lastmod
      implicit REAL*8 (a-h,o-z)
      dimension a(*),la(0:*),ws(*),li(*)
      integer rowi,coli,rowj,pri
      aiscpri2=0.d0
      jp=la(0)+coli
*jsp
      do j=la(jp),last(coli) !la(jp+1)-1
        rowj=la(j)
        if(rowj.eq.rowi)then
          aiscpri2=aiscpri2+di*a(j)
        else
          ir=li(rowj)-im
          if(ir.gt.0)goto1
          ir=ir+pri
          if(ir.gt.0)aiscpri2=aiscpri2+ws(ir)*a(j)
        endif
    1   continue
      enddo
      return
      end function

      subroutine update_L(pp,qq,n,nm,a,la,lr,lc,li,mao,p,q,r,s, 
     *  ws,mxws,d,sn,ifail)
	 use lastmod
      implicit REAL*8 (a-h,r-z), integer (i-q)
      dimension a(*),la(0:*),lr(*),lc(*),li(*),mao(*),
     *  p(*),q(*),r(*),s(*),ws(*),d(*),sn(*)
c     character star(1000,80)
      REAL*8 l11,l21
      integer r,s,rowim,rowi,rowj,rrj
      common/factorc/m1,nu,mp,mq,lastr,irow
      common/refactorc/nup,nfreq
      common/iprintc/iprint
      common/epsc/eps,tol,emin
      common/noutc/nout
      parameter (thresh=1.d-1,growth=1.d1)
c     write(nout,*)'update_L:  p,q =',pp,qq
      nup=nup+1
      if(qq.gt.n)then
        ilast=nu
        jp=la(0)+qq-n
        do j=la(jp),last(qq-n) !la(jp+1)-1
          ip=li(la(j))
          if(ip.gt.nu)ilast=max(ilast,mao(ip))
        enddo
        qqq=qq
      else
c  row flma procedure to remove row qq (includes qq amongst the unit vectors)
        iq=li(qq)
        if(iq.le.nu)goto99
        ilast=mao(iq)
        l11=1.d0
        u11=d(iq)
        ss=-sn(iq)
        nu=nu+1
        do i=iq,nu+1,-1
          lr(i)=lr(i-1)
          li(lr(i))=i
          sn(i)=sn(i-1)
          d(i)=d(i-1)
        enddo
        lr(nu)=qq
        li(qq)=nu
c  update mao
        do j=iq-1,nu,-1
          if(mao(j).lt.ilast)goto5
        enddo
        j=nu-1
    5   continue
        do j=j,nu,-1
          mao(j+1)=mao(j)+1
        enddo
        prq=p(qq)
        if(prq.gt.0)qrq=q(qq)
        do i=iq+1,ilast
          im=i-1
          rowi=lr(i)
          pri=p(rowi)
          u22=d(i)
          if(prq.gt.0)then
            u12=aiscpri2(n,a,la,qq,lc(i)-n,ws(qrq+1),l11,im,prq,li)
          else
            u12=l11*aij(qq,lc(i)-n,a,la)
          endif
          if(abs(u12).le.tol)u12=0.d0
          if(pri.gt.0)then
            qri=q(rowi)
            is=im-iq
            ii=pri-is
            if(ii.le.0)then
              l21=0.
            else
              l21=ws(qri+ii)
              if(abs(l21).le.tol)l21=0.d0
              if(ii.eq.1)then
                call trim(rowi,pri,qri,q,ws)
                if(pri.eq.0)call erase(rowi,lastr,irow,r,s)
                if(s(rowi).eq.0)then
                  qr_=mxws
                else
                  qr_=q(s(rowi))
                endif
                if(qri+pri.ge.qr_)then
                  call rshift(ws(qri),pri,1)
                  qri=qri-1
                  q(rowi)=qri
                endif
              else
                pri=pri-1
                call rshift(ws(qri+ii),is,1)
              endif
            endif
            p(rowi)=pri
          else
            l21=0.d0
          endif
          rr=-l21/l11
          del=rr*u12+u22
          test=abs(rr)*max(abs(u11),abs(u22))
c         write(nout,*)'l11,l21,u11,u12,u22,del,test',
c    *      l11,l21,u11,u12,u22,del,test
          is=pri-prq
          if(is.lt.0)test=test*growth
          if(u12.eq.0.d0.and.is.gt.0)test=test*thresh
c           write(nout,*)'rowi,pri,qri =',rowi,pri,qri
c           write(nout,*)'rowq,prq,qrq =',qq,prq,qrq
c           write(nout,*)'j,p(j),q(j),r(j),s(j)   irow =',irow
c           do j=1,n
c             if(p(j).ne.0)write(nout,*)j,p(j),q(j),r(j),s(j)
c           enddo
c           write(nout,*)'rowq =',(ws(qrq+ij),ij=1,prq)
c           write(nout,*)'rowi =',(ws(qri+ij),ij=1,pri)
          if(abs(del).le.test)then
c  no-perm operation for row flma
c           write(nout,*)'no-perm operation for row flma'
            if(is.gt.0)then
              pr_=prq
              prq=pri+1
              call newslot(qq,prq,lastr,irow,p,q,r,s,ws,mxws,qr_,ifail)
              if(ifail.gt.0)return
              qrq=q(qq)
              qri=q(rowi)
	nsift=qri-qrq
	if(nsift.ge.0)then
!call rshift(ws(qrq+1),pri,qri-qrq)
              call rshift(ws(qrq+1),pri,nsift)
	else
          call rlshift(ws,qrq+1,pri,nsift)

	endif

              call mysaxpy(rr,ws(qr_+1),ws(qri+is+1),pr_)
            else
              if(prq.eq.0)then
                call erase(rowi,lastr,irow,r,s)
                p(rowi)=0
                call newslot(qq,1,lastr,irow,p,q,r,s,ws,mxws,qr_,ifail)
                if(ifail.gt.0)return
                prq=1
                qrq=q(qq)
              else
                is=-is
                do j=1,is
                  ws(qrq+j)=rr*ws(qrq+j)
                enddo
                if(pri.gt.0)then
                  call saxpyx(rr,ws(qrq+is+1),ws(qri+1),pri)
                else
                  call newslot(rowi,1,lastr,irow,p,q,r,s,ws,mxws,qr_,
     *              ifail)
                  if(ifail.gt.0)return
                  qri=q(rowi)
                  qrq=q(qq)
                endif
                if(abs(ws(qrq+1)).le.tol)call trim(qq,prq,qrq,q,ws)
c  rename qq as rowi and vice-versa
                if(qri.lt.qrq)then
                  if(s(rowi).eq.qq)then
                    r(qq)=r(rowi)
                    r(rowi)=qq
                    s(rowi)=s(qq)
                    s(qq)=rowi
                  else
                    call iexch(r(qq),r(rowi))
                    call iexch(s(qq),s(rowi))
                    r(s(qq))=qq
                    s(r(rowi))=rowi
                  endif
                  if(r(qq).gt.0)then
                    s(r(qq))=qq
                  else
                    irow=qq
                  endif
                  if(s(rowi).gt.0)r(s(rowi))=rowi
                else
                  if(s(qq).eq.rowi)then
                    r(rowi)=r(qq)
                    r(qq)=rowi
                    s(qq)=s(rowi)
                    s(rowi)=qq
                  else
                    call iexch(r(rowi),r(qq))
                    call iexch(s(rowi),s(qq))
                    r(s(rowi))=rowi
                    s(r(qq))=qq
                  endif 
                  if(r(rowi).gt.0)then
                    s(r(rowi))=rowi
                  else
                    irow=rowi
                  endif
                  if(s(qq).gt.0)r(s(qq))=qq
                endif
                call iexch(pri,prq)
                call iexch(qri,qrq)
                call iexch(q(rowi),q(qq))
                if(pri.eq.0)call erase(rowi,lastr,irow,r,s)
                prq=prq+1
              endif
            endif
            p(rowi)=pri
            p(qq)=prq
            ws(qrq+prq)=1.d0
            d(i)=rr*u11
            u11=u22
            l11=l21
          else
c  perm operation for row flma
c           write(nout,*)'perm operation for row flma'
            if(rr.ne.0.d0)then
              if(is.ge.0)then
                if(prq.gt.0)then
                  call mysaxpy(rr,ws(qrq+1),ws(qri+is+1),prq)
                  if(abs(ws(qri+1)).le.tol)call trim(rowi,pri,qri,q,ws)
                  if(pri.eq.0)call erase(rowi,lastr,irow,r,s)
                endif
                is=pri-prq
              else
                pr_=pri
                pri=prq
                call newslot(rowi,pri,lastr,irow,p,q,r,s,ws,mxws,qr_,
     *            ifail)
                if(ifail.gt.0)return
                qrq=q(qq)
                qri=q(rowi)
                is=-is
                do j=1,is
                  ws(qri+j)=rr*ws(qrq+j)
                enddo
                call saxpyz(rr,ws(qrq+is+1),ws(qr_+1),ws(qri+is+1),pr_)
                is=0
              endif
            endif
            p(rowi)=pri
            if(u12.ne.0.d0)then
              u12=-u12/del
              if(is.gt.0)then
                pr_=prq
                prq=pri+1
                call newslot(qq,prq,lastr,irow,p,q,r,s,ws,mxws,qr_,
     *            ifail)
                if(ifail.gt.0)return
                qrq=q(qq)
                qri=q(rowi)
                do j=1,is
                  ws(qrq+j)=u12*ws(qri+j)
                enddo
                call saxpyz(u12,ws(qri+is+1),ws(qr_+1),ws(qrq+is+1),pr_)
                ws(qrq+prq)=u12
                goto7
              else
                if(pri.gt.0)then
                  is=-is
                  call mysaxpy(u12,ws(qri+1),ws(qrq+is+1),pri)
                  if(abs(ws(qrq+1)).le.tol)then
                    call trim(qq,prq,qrq,q,ws)
                    if(prq.eq.0)call erase(qq,lastr,irow,r,s)
                    p(qq)=prq
                  endif
                endif
              endif
            endif
            if(prq.gt.0.or.u12.ne.0.d0)then
              if(prq.eq.0)then
                len=0
              elseif(s(qq).eq.0)then
                len=mxws-qrq
              else
                len=q(s(qq))-qrq
              endif
              if(len.eq.prq)then
                call newslot(qq,prq+1,lastr,irow,p,q,r,s,ws,mxws,qr_,
     *            ifail)     
                if(ifail.gt.0)return
                qrq=q(qq)
                qri=q(rowi)
	nsift=qr_-qrq
	if(nsift.ge.0)then
!                call rshift(ws(qrq+1),prq,qr_-qrq)
        call rshift(ws(qrq+1),prq,qnsift)
	else
       call rlshift(ws,qrq+1,prq,nsift)

	endif
              endif
              prq=prq+1
              ws(qrq+prq)=u12
            endif
   7        continue
            p(rowi)=pri
            p(qq)=prq
            d(i)=del
            u11=u11*u22/del
            call iexch(lc(i),lc(im))
          endif
c           write(nout,*)'rowi,pri,qri =',rowi,pri,qri
c           write(nout,*)'rowq,prq,qrq =',qq,prq,qrq
c           write(nout,*)'j,p(j),q(j),r(j),s(j)   irow =',irow
c           do j=1,n
c             if(p(j).ne.0)write(nout,*)j,p(j),q(j),r(j),s(j)
c           enddo
c           write(nout,*)'rowq* =',(ws(qrq+ij),ij=1,prq)
c           write(nout,*)'rowi* =',(ws(qri+ij),ij=1,pri)
        enddo
        if(prq.gt.0)then
c         write(nout,*)'ss,l11,ilast,n,prq',ss,l11,ilast,n,prq
c         write(nout,*)'sn =',(sn(ij),ij=nu+1,n)
          call mysaxpy(ss/l11,ws(qrq+1),sn(ilast-prq+1),prq)
          call erase(qq,lastr,irow,r,s)
          p(qq)=0
        endif
        qqq=lc(ilast)
        do i=ilast,nu+1,-1
          lc(i)=lc(i-1)
          li(lc(i))=i
        enddo
c       if(pp.le.n)then
c         ip=li(pp)
c         write(nout,*)'check sn'
c         do i=nu+1,ilast
c           nodec=lc(i)
c           u12=aiscpri2(n,a,la,pp,lc(i)-n,sn(nu+1),1.E0,ilast,
c             ilast-nu,li)
c           if(abs(u12).gt.tol)write(nout,*)'error,nodec =',u12,nodec
c         enddo
c       endif
c       write(nout,*)'intermediate PAQ factors:  new q =',qqq
c       write(nout,*)'lr =',(lr(j),j=nu+1,n)
c       write(nout,*)'lc =',(lc(j),j=nu+1,n)
c       write(nout,*)'d =',(d(ij),ij=nu+1,n)
c       do j=nu+1,n
c         rowj=lr(j)
c         if(p(rowj).ne.0)then
c           write(nout,*)'L(',rowj,')',
c    *        (ws(k),k=q(rowj)+1,q(rowj)+p(rowj))
c         endif
c       enddo
c       call checkout(n,a,la,lr,lc,li,p,q,r,s,ws,mxws,d)
      endif
      ip=li(pp)
      if(pp.gt.n)then
        li(pp)=0
        if(pp.eq.qqq)goto30
        if(ip.le.nu)goto99
        iout=ip
        rowim=lr(ip)
        prim=p(rowim)
        if(prim.gt.0)qrim=q(rowim)
      else
        if(ip.gt.nu.or.p(pp).gt.0)goto99
        lr(ip)=lr(nu)
        li(lr(ip))=ip
c  check for growth in sn
c       write(nout,*)'sn =',(sn(i),i=nu+1,n)
        iout=ilast
        i=nu+1
        if(i.gt.ilast)goto13
   11   continue
          do j=i,mao(i)
            if(abs(sn(j)).gt.growth)then
              iout=i-1
              goto13
            endif
          enddo
          i=mao(i)+1
          if(i.le.ilast)goto11
   13   continue
        do j=nu+1,iout
          if(abs(sn(j)).gt.tol)goto14
        enddo
        j=iout+1
   14   continue
        rowim=pp
        prim=iout-j+1
        if(prim.gt.0)then
          call newslot(pp,prim,lastr,irow,p,q,r,s,ws,mxws,qr_,ifail)
          if(ifail.gt.0)return
          p(pp)=prim
          qrim=q(pp)
          ii=qrim
          do j=j,iout
            ii=ii+1
            ws(ii)=sn(j)
          enddo
        endif
        do i=nu,iout-1
          lr(i)=lr(i+1)
          li(lr(i))=i
          lc(i)=lc(i+1)
          li(lc(i))=i
          d(i)=d(i+1)
        enddo
        lr(iout)=pp
        li(pp)=iout
c       write(nout,*)'lr =',(lr(ij),ij=nu,iout)
c       write(nout,*)'lc =',(lc(ij),ij=nu,iout-1)
c       if(prim.gt.0)write(nout,*)'L(',pp,') =',(ws(qrim+j),j=1,prim)
        nu=nu-1
      endif
c     write(nout,*)'iout,ilast,rowim,prim =',iout,ilast,rowim,prim
c  column flma operations to restore L to triangular form
      iswap=0
      do i=iout+1,ilast
        im=i-1
        lc(im)=lc(i)
        li(lc(im))=im
        rowi=lr(i)
        pri=p(rowi)
c       if(pri.gt.0)write(nout,*)'L(',rowi,') =',(ws(q(rowi)+j),j=1,pri)
        u22=d(i)
        if(prim.gt.0)then
          u12=aiscpri2(n,a,la,rowim,lc(i)-n,ws(qrim+1),1.d0,im-1,prim,
     *      li)
          if(abs(u12).le.tol)u12=0.d0
        else
          u12=aij(rowim,lc(i)-n,a,la)
        endif
        if(pri.gt.0)then
c         write(nout,*)'pri,iswap',pri,iswap
          qri=q(rowi)
          ii=pri-iswap
          if(ii.le.0)then
            l21=0.d0
          else
            l21=ws(qri+ii)
            if(abs(l21).le.tol)l21=0.d0
            if(ii.eq.1)then
              call trim(rowi,pri,qri,q,ws)
              if(pri.eq.0)call erase(rowi,lastr,irow,r,s)
              if(s(rowi).eq.0)then
                qr_=mxws
              else
                qr_=q(s(rowi))
              endif
              if(qri+pri.ge.qr_)then
                call rshift(ws(qri),pri,1)
                qri=qri-1
                q(rowi)=qri
              endif
            else
              pri=pri-1
              call rshift(ws(qri+ii),iswap,1)
            endif
            p(rowi)=pri
c           write(nout,*)'rowi =',(ws(qri+ij),ij=1,pri)
          endif
        else
          l21=0.d0
        endif
        del=u22-l21*u12
        test=abs(u12)*max(1.d0,abs(l21))
c       write(nout,*)'l21,u12,u22,del,test',l21,u12,u22,del,test
        is=pri-prim
        if(is.gt.0)test=growth*test
        if(l21.eq.0.d0.and.is.lt.0)test=thresh*test
c         write(nout,*)'rowim,prim,qrim =',rowim,prim,qrim
c         write(nout,*)'rowi,pri,qri =',rowi,pri,qri
c         write(nout,*)'j,p(j),q(j),r(j),s(j)   irow =',irow
c         do j=1,n
c           if(p(j).ne.0)write(nout,*)j,p(j),q(j),r(j),s(j)
c         enddo
c         write(nout,*)'rowim =',(ws(qrim+ij),ij=1,prim)
c         write(nout,*)'rowi =',(ws(qri+ij),ij=1,pri)
        if(abs(del).le.test)then
c  no-perm operation for column flma
c         write(nout,*)'no-perm operation for column flma'
          rr=-u22/u12
          l21=l21+rr
          if(abs(l21).le.tol)l21=0.d0
          if(is.ge.0)then
            if(prim.gt.0)then
              call mysaxpy(rr,ws(qrim+1),ws(qri+is+1),prim)
              if(abs(ws(qri+1)).le.tol)call trim(rowi,pri,qri,q,ws)
              if(pri.eq.0)then
                call erase(rowi,lastr,irow,r,s)
                p(rowi)=0
              endif
            endif
            if(pri.gt.0.or.l21.ne.0.d0)then
              if(pri.eq.0)then
                len=0
              elseif(s(rowi).eq.0)then
                len=mxws-qri
              else
                len=q(s(rowi))-qri
              endif
              if(len.eq.pri)then
                call newslot(rowi,pri+1,lastr,irow,p,q,r,s,ws,mxws,qr_,
     *            ifail)
                if(ifail.gt.0)return
                qrim=q(rowim)
                qri=q(rowi)
	nsift=qr_-qri
	if(nsift.ge.0)then
 !               call rshift(ws(qri+1),pri,qr_-qri)
	call rshift(ws(qri+1),pri,nsift)
	else
       call rlshift(ws,qri+1,pri,nsift)

	endif
              endif
              pri=pri+1
              ws(qri+pri)=l21
            endif
          else
            pr_=pri
            pri=prim+1
            call newslot(rowi,pri,lastr,irow,p,q,r,s,ws,mxws,qr_,ifail)      
            if(ifail.gt.0)return
            qrim=q(rowim)
            qri=q(rowi)
            is=-is
            do j=1,is
              ws(qri+j)=rr*ws(qrim+j)
            enddo
!	write(16,*)'bug qr_+1,mxws,rowi,s(rowi),w(s(rowi)',qr_+1,
!     2	mxws,rowi,s(rowi)
!      if(s(rowi).ne.0)write(16,*) q(s(rowi))
            call saxpyz(rr,ws(qrim+is+1),ws(qr_+1),ws(qri+is+1),pr_)
            ws(qri+pri)=l21
          endif
c           write(nout,*)'rowim,prim,qrim =',rowim,prim,qrim
c           write(nout,*)'rowi,pri,qri =',rowi,pri,qri
c           write(nout,*)'j,p(j),q(j),r(j),s(j)   irow =',irow
c           do j=1,n
c             if(p(j).ne.0)write(nout,*)j,p(j),q(j),r(j),s(j)
c           enddo
c           write(nout,*)'rowim* =',(ws(qrim+ij),ij=1,prim)
c           write(nout,*)'rowi* =',(ws(q(rowi)+ij),ij=1,p(rowi))
          p(rowi)=pri
          rowim=rowi
          prim=pri
          qrim=qri
          d(im)=u12
c  perform accumulated cyclic permutation in subsequent rows
          if(iswap.gt.0)then
            do j=i+1,ilast
              rowj=lr(j)
              prj=p(rowj)
              is=prj-j+i
              if(is.gt.0)then
                qrj=q(rowj)
                if(is.gt.iswap)then
                  ii=is-iswap
                  l21=ws(qrj+ii)
                  call rshift(ws(qrj+ii),iswap,1)
                  ws(qrj+is)=l21
                  if(abs(ws(qrj+1)).le.tol)call trim(rowj,prj,qrj,q,ws)
                  if(prj.eq.0)call erase(rowj,lastr,irow,r,s)
                else
                  prj=prj+1
                  rrj=r(rowj)
                  if(rrj.eq.0)then
                    len=qrj
                  else
                    len=qrj-q(rrj)-p(rrj)
                  endif
                  if(len.gt.0)then
                    call rshift(ws(qrj),is,1)
                    ws(qrj+is)=0.d0
                    qrj=qrj-1
                    q(rowj)=qrj
                  else
                    call newslot(rowj,prj,lastr,irow,p,q,r,s,ws,mxws,
     *                qr_,ifail) 
                    if(ifail.gt.0)return
                    qrj=q(rowj)
                    qrim=q(rowim)

	nsift=qr_-qrj
	if(nsift.ge.0)then
 !                   call rshift(ws(qrj+1),is,qr_-qrj)
             call rshift(ws(qrj+1),is,nsift)
       else
              call rlshift(ws,qrj+1,is,nsift)

	endif


                    ws(qrj+is+1)=0.d0
	nsift=qr_-qrj-1
	if(nsift.ge.0)then
 !     call rshift(ws(qrj+is+2),j-i,qr_-qrj-1)
                    call rshift(ws(qrj+is+2),j-i,nsift)

         else
        call rlshift(ws,qrj+is+2,j-i,nsift)

	endif


                  endif
                endif
                p(rowj)=prj
c               write(nout,*)'L(',rowj,')* =',(ws(qrj+ij),ij=1,prj)
              endif
            enddo
          endif
          iswap=0
        else
c  perm operation for column flma
c         write(nout,*)'perm operation for column flma'
          rr=-l21
          if(rr.ne.0.d0)then
            if(is.ge.0)then
              if(prim.gt.0)then
                call mysaxpy(rr,ws(qrim+1),ws(qri+is+1),prim)
                if(abs(ws(qri+1)).le.tol)call trim(rowi,pri,qri,q,ws)
                if(pri.eq.0)call erase(rowi,lastr,irow,r,s)
              endif
              is=pri-prim
            else
              pr_=pri
              pri=prim
              call newslot(rowi,pri,lastr,irow,p,q,r,s,ws,mxws,qr_,
     *          ifail)
              if(ifail.gt.0)return
              qrim=q(rowim)
              qri=q(rowi)
              is=-is
              do j=1,is
                ws(qri+j)=rr*ws(qrim+j)
              enddo
              call saxpyz(rr,ws(qrim+is+1),ws(qr_+1),ws(qri+is+1),pr_)
              is=0
            endif
          endif
          p(rowi)=pri
          if(u12.ne.0.d0)then
            u12=-u12/del
            if(is.gt.0)then
              pr_=prim
              prim=pri+1
              call newslot(rowim,prim,lastr,irow,p,q,r,s,ws,mxws,qr_,
     *          ifail)
              if(ifail.gt.0)return
              qrim=q(rowim)
              qri=q(rowi)
              do j=1,is
                ws(qrim+j)=u12*ws(qri+j)
              enddo
              call saxpyz(u12,ws(qri+is+1),ws(qr_+1),ws(qrim+is+1),pr_)
              ws(qrim+prim)=u12
              goto27
            else
              if(pri.gt.0)then
                is=-is
                call mysaxpy(u12,ws(qri+1),ws(qrim+is+1),pri)
                if(abs(ws(qrim+1)).le.tol)then
                  call trim(rowim,prim,qrim,q,ws)
                  if(prim.eq.0)call erase(rowim,lastr,irow,r,s)
                  p(rowim)=prim
                endif
              endif
            endif
          endif
          if(prim.gt.0.or.u12.ne.0.d0)then
            if(prim.eq.0)then
              len=0
            elseif(s(rowim).eq.0)then
              len=mxws-qrim
            else
              len=q(s(rowim))-qrim
            endif
            if(len.eq.prim)then
              call newslot(rowim,prim+1,lastr,irow,p,q,r,s,ws,mxws,qr_,
     *          ifail)
              if(ifail.gt.0)return
              qrim=q(rowim)
              qri=q(rowi)
	nsift=qr_-qrim
!              call rshift(ws(qrim+1),prim,qr_-qrim)
         if(nsift.ge.0)then
	 call rshift(ws(qrim+1),prim,nsift)
            else
           call rlshift(ws,qrim+1,prim,nsift)
	endif

            endif
            prim=prim+1
            ws(qrim+prim)=u12
          endif
   27     continue
          p(rowim)=prim
          p(rowi)=pri
c           write(nout,*)'rowim,prim,qrim =',rowim,prim,qrim
c           write(nout,*)'rowi,pri,qri =',rowi,pri,qri
c           write(nout,*)'j,p(j),q(j),r(j),s(j)   irow =',irow
c           do j=1,n
c             if(p(j).ne.0)write(nout,*)j,p(j),q(j),r(j),s(j)
c           enddo
c           write(nout,*)'rowim* =',(ws(qrim+ij),ij=1,prim)
c           write(nout,*)'rowi* =',(ws(q(rowi)+ij),ij=1,p(rowi))
          d(im)=del
          call iexch(lr(i),lr(i-1))
          call iexch(li(lr(i)),li(lr(i-1)))
          iswap=iswap+1
        endif
      enddo
      lc(ilast)=qqq
      li(qqq)=ilast
c     write(nout,*)'rowim* =',(ws(qrim+ij),ij=1,prim)
c     write(nout,*)'ilast,prim,qrim',ilast,prim,qrim
      if(prim.gt.0)then
       d(ilast)=aiscpri2(n,a,la,rowim,qqq-n,ws(qrim+1),1.d0,ilast-1,
     *    prim,li)
      else
        d(ilast)=aij(rowim,qqq-n,a,la)
      endif
c  reset mao
      iout=ilast
      do i=ilast,nu+1,-1
        mao(i)=ilast
        iout=min(iout,i-p(lr(i)))
        if(iout.eq.i)ilast=i-1
      enddo
   30 continue
      m1=n-nu
c     write(nout,*)'PAQ factors:  nu =',nu
c     write(nout,*)'d =',(d(ij),ij=nu+1,n)
c     do j=nu+1,n
c       rowj=lr(j)
c       if(p(rowj).ne.0)then
c         write(nout,*)'L(',rowj,')',
c    *      (ws(k),k=q(rowj)+1,q(rowj)+p(rowj))
c       endif
c     enddo
c     call checkout(n,a,la,lr,lc,li,p,q,r,s,ws,mxws,d)
c  print star diagram
c     if(m1.gt.80.or.n.gt.1000)stop
c     write(nout,*)'updated ordering:  nu =',nu
c     do i=1,n
c       do j=1,m1
c         star(i,j)=' '
c       enddo
c     enddo
c     do j=1,m1
c       jp=la(0)+lc(nu+j)-n
c       do i=la(jp),la(jp+1)-1
c         star(li(la(i)),j)='*'
c       enddo
c     enddo
c     do i=nu+1,n
c       write(nout,*)(star(i,j),j=1,m1)
c     enddo
c     write(nout,*)'lr =',(lr(j),j=nu+1,n)
c     write(nout,*)'lc =',(lc(j),j=nu+1,n)
c     write(nout,*)'mao =',(mao(j),j=nu+1,n)
      return
   99 continue
      write(nout,*)'malfunction in update_L:  p,q =',pp,qq
      stop
      end subroutine

      subroutine newslot(row,len,lastr,irow,p,q,r,s,ws,mxws,qr_, 
     *  ifail)
      implicit REAL*8 (a-h,u-z), integer (i-t)
      parameter (igap=10)
      dimension p(*),q(*),r(*),s(*),ws(*)
      common/noutc/nout
c     write(nout,*)'newslot: row =',row,'   len =',len
c     write(nout,*)'irow,lastr,mxws =',irow,lastr,mxws
      ifail=0
      if(lastr.eq.0)then
        if(mxws.lt.len)then
          write(*,*)'insufficient space available for profile'
          ifail=7
        else
          irow=row
          q(row)=0
          r(row)=0
          s(row)=0
          lastr=row
        endif
        return
      endif
      igp=igap
    1 continue
      len_=len+igp
      thisr=lastr
    2 continue
      
      qrow=q(thisr)+p(thisr)
      nextr=s(thisr)
c     write(nout,*)'thisr,nextr,qrow,p(thisr),len_',
c    *  thisr,nextr,qrow,p(thisr),len_
      if(nextr.ne.0)then
        if(q(nextr).ge.qrow+len_)then
c  free slot after this row
          goto4
        else
          thisr=nextr
          if(thisr.ne.lastr)goto2
        endif
      else
        if(mxws-qrow.ge.len_)then
c  free slot at end of ws
          goto4
        elseif(q(irow).ge.len_)then
c  free slot at beginning of ws
          qrow=0
          thisr=0
          nextr=irow
          irow=row
          igp=0
          goto4
        endif
        thisr=irow
        if(thisr.ne.lastr)goto2
      endif
c  no free space: try minimum value of len
      if(igp.gt.0)then
        igp=0
        goto1
      endif
c  compress ws
      thisr=irow
      qrow=0
    3 continue
       nsift=q(thisr)-qrow
	if(nsift.ge.0)then
!	call rshift(ws(qrow+1),p(thisr),q(thisr)-qrow)
      call rshift(ws(qrow+1),p(thisr),nsift)
	else
        call rlshift(ws,qrow+1,p(thisr),nsift)

	endif
      q(thisr)=qrow
      qrow=qrow+p(thisr)
      if(s(thisr).ne.0)then
        thisr=s(thisr)
        goto3
      endif
      if(mxws.lt.qrow+len_)then
        write(nout,*)'insufficient space available for profile'
        !20130222 oli: write(nout,*)'mxws,qrow,len_',mxws,qrow,len_
        ifail=7
        return
      endif
c  insert at end of compressed file
      nextr=0
    4 continue
      qr_=q(row)
      q(row)=qrow+igp
      if(p(row).gt.0)then
        if(r(row).eq.thisr.or.s(row).eq.nextr)return
c  insert after row thisr and take out old row
        call erase(row,lastr,irow,r,s)
      endif
      lastr=row
      r(row)=thisr
      if(thisr.gt.0)s(thisr)=row
      s(row)=nextr
      if(nextr.gt.0)r(nextr)=row
      i=0
      return
      end subroutine

      subroutine erase(row,lastr,irow,r,s) 
c  remove slot for row from the data file
      implicit integer (i-s)
      dimension r(*),s(*)
      common/noutc/nout
c     write(nout,*)'erase: row,irow,lastr =',row,irow,lastr
      if(r(row).eq.0)then
        if(s(row).eq.0)then
          irow=0
          lastr=0
          return
        endif
        irow=s(row)
        r(irow)=0
      elseif(s(row).eq.0)then
        s(r(row))=0
      else
        s(r(row))=s(row)
        r(s(row))=r(row)
      endif
      if(row.eq.lastr)lastr=irow
      return
      end subroutine

      subroutine trim(rowi,pri,qri,q,ws) 
c  trim leading zeros off slot for row i
      implicit REAL*8 (a-h,s-z), integer (i-r)
      dimension q(*),ws(*)
      common/epsc/eps,tol,emin
    1 continue
      qri=qri+1
      pri=pri-1
      if(pri.eq.0)return
      if(abs(ws(qri+1)).le.tol)goto1
      q(rowi)=qri
      return
      end subroutine

      subroutine checkout(n,a,la,lr,lc,li,p,q,r,s,ws,mxws,d) 
      implicit REAL*8 (a-h,r-z), integer (i-q)
      integer r,s,rowj,thisr
      dimension a(*),la(*),lr(*),lc(*),li(*),p(*),q(*),r(*),s(*),ws(*),
     *  d(*)
      common/factorc/m1,nu,mp,mq,lastr,irow
      common/noutc/nout
      common/epsc/eps,tol,emin
c  check indexing
      do j=1,nu
        if(p(lr(j)).ne.0)then
          write(nout,*)'p(lr(j)).ne.0'
          goto11
        endif
      enddo
      np=0
      do i=nu+1,n
        if(p(lr(i)).gt.0)np=np+1
      enddo
      if(irow.gt.0)then
        if(r(irow).ne.0)then
          write(nout,*)'r(irow).ne.0'
          goto11
        endif
        thisr=irow
    1   continue
        if(p(thisr).le.0)then
          write(nout,*)'p(thisr).le.0'
          goto11
        endif
        np=np-1
        nextr=s(thisr)
        if(nextr.eq.0)then
          if(q(thisr)+p(thisr).gt.mxws)then
            write(nout,*)'q(thisr)+p(thisr).gt.mxws'
            goto11
          endif
        else
          if(r(nextr).ne.thisr)then
            write(nout,*)'r(nextr).ne.thisr'
            goto11
          endif
          if(nextr.ne.s(thisr))then
            write(nout,*)'nextr.ne.s(thisr)'
            goto11
          endif
          if(q(thisr)+p(thisr).gt.q(nextr))then
            write(nout,*)'q(thisr)+p(thisr).gt.q(nextr)'
            goto11
          endif
          thisr=nextr
          goto1
        endif
      endif
      if(np.ne.0)then
        write(nout,*)'np.ne.0'
        goto11
      endif
      last=0
      emax=0.d0
      length=0
      do inode=nu+1,n
        nodec=lc(inode)
c  form L.a_q
        rowj=lr(inode)
        prj=p(rowj)
        length=length+prj
        if(prj.lt.0)then
          write(nout,*)'prj.lt.0'
          goto11
        elseif(prj.eq.0)then
          e=abs(aij(rowj,nodec-n,a,la)-d(inode))
        else
          e=abs(d(inode)-aiscpri2(n,a,la,rowj,nodec-n,ws(q(rowj)+1),
     *      1.d0,inode-1,prj,li))
        endif
c       if(e.gt.tol)write(nout,*)'error =',e,
c    *    '  inode,nodec,rowj =',inode,nodec,rowj
        emax=max(emax,e)
        do j=inode+1,n
          rowj=lr(j)
          prj=p(rowj)
          if(prj.gt.0)then
            e=abs(aiscpri2(n,a,la,rowj,nodec-n,ws(q(rowj)+1),1.d0,j-1,
     *         prj,li))
          else
            e=abs(aij(rowj,nodec-n,a,la))
          endif
c         if(e.gt.tol)write(nout,*)'error =',e,
c    *      '  inode,nodec,j,rowj =',inode,nodec,j,rowj
          emax=max(emax,e)
        enddo
      enddo
      write(nout,*)'checkout:  m1 =',m1,'  file length =',length
      if(emax.gt.tol)write(nout,*)'error =',emax
      return
   11 continue
      write(nout,*)'thisr,nextr =',thisr,nextr
      write(nout,*)'i,p(i),q(i),r(i),s(i):  irow =',irow
      do i=1,n
        if(p(i).ne.0)write(nout,*)i,p(i),q(i),r(i),s(i)
      enddo
      stop
      end subroutine






	end module
	      subroutine stmap(n,nm,kmax) 
c  set storage map for workspace in bqpd and auxiliary routines
      implicit REAL*8 (a-h,r-z), integer (i-q)
      common/wsc/kk,ll,kkk,lll,mxws,mxlws
      common/bqpdc/irh1,na,na1,nb,nb1,ka1,kb1,kc1,irg1,lu1,lv,lv1,ll1
c  real storage (ws)
      kkk=kmax*(kmax+9)/2+nm+n
c  (number of real locations required by bqpd)
c  slot for user workspace for gdotx
      irh1=kk+1
c  slot of length kmax*(kmax+1)/2 for reduced Hessian matrix
      na1=irh1+kmax*(kmax+1)/2
      na=na1-1
c  scratch slots of length n+m, n and four of length kmax follow
      nb1=na1+nm
      nb=nb1-1
      ka1=nb1+n
      kb1=ka1+kmax
      kc1=kb1+kmax
      irg1=kc1+kmax
      lu1=irg1+kmax
c  remaining space for use by denseL.f or sparseL.f
c  integer storage (lws)
      lll=kmax
c  (number of integer locations required by bqpd)
c  slot for user workspace for gdotx
      lv=ll
      lv1=ll+1
c  slot for V-list
      ll1=lv1+kmax
c  remaining space for use by denseL.f or sparseL.f
       !20130222 oli: write(6,*)'stmap,n,nm,kmax,kk,lu1,ll',n,nm,kmax,kk,lu1,ll
      return
      end subroutine




      
