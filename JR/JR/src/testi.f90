
subroutine testi(remain,tulos)
	logical,intent(in)::remain
        double precision,intent(out)::tulos
        tulos=0
        if (remain) then 
           tulos=1
        end if
        end subroutine