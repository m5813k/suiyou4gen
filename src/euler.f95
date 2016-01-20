program Euler
implicit none
real(8) :: dt, t,    u
integer :: n,  nend, np

write(*, *) ' input data: dt,nend, np'
read(*,  *)  dt, nend, np
write(*, *) 'dt=',dt,' nend=',nend,' np=',np

u    = 1.0d0
do n = 1, nend
    t    = dt*real(n)
    u    = u*(1.0d0+dt)
    if (mod(n,np)==0) write(*,'(f7.3,2d15.7)') t, u, exp(t)
end do 

end
