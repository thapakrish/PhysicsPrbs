!		@author Krishna


!		prints data for mercury orbit with 0.39 eccentricity
		program mercury
		real *4 vx(10000), vy(10000), x(10000), y(10000), r(10000), dt, yr
		parameter(au=0.39, pi=3.14159265358979)
		integer i, j, k
		x(1)=0.39*(1+0.206) 		
		y(1)=0.0
		vx(1)=0.0
		yr=1.0
		vy(1)=10.06
		print *, x(1), y(1), vy(1), vx(1)
		print *, 'time step'		
			read(5,*) dt
		do 100 i=1, 10000	
			r(i)=sqrt(x(i)**2+y(i)**2)
		vx(i+1)=vx(i)-(4*pi**2*x(i)*dt)/(r(i))**3
		vy(i+1)=vy(i)-(4*pi**2*y(i)*dt)/(r(i))**3
		x(i+1)=x(i)+vx(i+1)*dt
		y(i+1)=y(i)+vy(i+1)*dt
		if ((x(i+1).GT.x(1)).AND.(y(i+1).GT.0)) then
			exit
		end if
100		continue
		
					OPEN(UNIT=1,FILE='mercury.dat',STATUS='UNKNOWN')
						do j=1,10000
						write(1, *) x(j), y(j), r(j)
						end do		
					CLOSE(1)
			
			
		end 
!		Go to terminal
!		gfortran mercury.f90
!		./a.out
!		give time step: 0.001? 0.01? 0.005? Your choice
!		go to gnuplot
! 		load mercury.p		