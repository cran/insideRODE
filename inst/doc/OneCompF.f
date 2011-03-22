c file mymodf.f
       subroutine initmod(odeparms)
       external odeparms
       double precision parms(3)
       common /myparms/parms
	 call odeparms(2, parms)
       return
       end
       subroutine derivs (neq, t, y, ydot, yout, ip)
        double precision t, y, ydot, ka, ke, CL
        integer neq, ip(*)
        dimension y(2), ydot(2), yout(2)
        common /myparms/ka,ke,CL
        if(ip(1) < 1) call rexit("nout should be at least 1")
        ydot(1) = -exp(ka)*y(1)
        ydot(2) = exp(ka)*y(1)-exp(ke)*y(2)
        yout(1) = 0
        yout(2) = y(2)/exp(CL)*exp(ke)
        return
        end