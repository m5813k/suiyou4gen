program Euler2
implicit none
real(8) :: dt, t,    u1, u2, utemp
integer :: n,  nend, np

write(*, *) ' input data: dt,nend, np'
read(*,  *)  dt,   nend, np
write(*, *) 'dt=', dt,   ' nend=', nend, ' np=', np

u1 = 1.0d0
u2 = (1 + dt)*u1

do n = 1, nend
    t     = dt*real(n)
    utemp = u2
    u2    = 2.0d0*dt*u1 + u2
    u1    = utemp
    if (mod(n,np)==0) write(*,'(f7.3,2d15.7)') t, u2, exp(t)
end do 

end
