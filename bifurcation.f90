      	program logistic

		double precision mu(5000), mu_stp, x(5000),b(5000),tmp(5000)
		character ch*2
		integer n,i,j,k,l,m, count
		count=1
		print *, 'no. of iterations'
			read(5,*) n
!		n=100
!		print *, 'mu value'
!			read (5,*) mu	
		print *, 'mu steps'
			read (5,*) mu_stp
		m=int(4/mu_stp)
!		print *, m
		print *, 'x-initial'
			read(5,*) x(1)
		mu(1)=2.0
		do 10 i=1, m
			do 20 j=1, n
				x(j+1)=mu(1)*x(j)*(1-x(j))
				
			where(j.EQ.150)
!				if (j.EQ.150) then					
!					do k=1,8
!						if(x(j-k).EQ.x(j)) then
!							b(count)=x(j)
!							tmp(count)=mu(i)
!							count=count+1
!						else if (x(j-k).EQ.x(j-2)) then
!							b(count)=x(j)
!							tmp(count)=mu(i)
!							count=count+1
!						end if 
!					end do
!				end if 

				
			end	
				
			mu(i+1)=mu(i)+mu_stp
20			continue

!	  	if (i.LT.10) then
 !      			WRITE(ch,'(a1,i1)')'0',i  	!for file 01, 02,...,09
  !    		else

!	 	WRITE(ch,'(i2)')i         	! for file 11, 12,...,K
!		        end if

		OPEN(UNIT=1,FILE='hw3b.dat',STATUS='UNKNOWN')
				do l=1,n-1
					write(1, 30) b(l), tmp(l)
				end do	
			close(1)
30		    	format(1x,1p,4(e12.5,2x))

!		      	return
		mu=mu+mus
10		continue		!end do 
		print *, mu
		return

		stop
		end
!starts @4.001
