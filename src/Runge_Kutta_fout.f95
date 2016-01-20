program Runge_Kutta
implicit none
real(8) :: dt, t,    u, p1, p2, p3,p4
integer :: n,  nend, np, nfout
character(130)::fout

write(*, *) ' input data: dt,nend, np'
read(*,  *)  dt,   nend, np
write(*, *) 'dt=', dt,   ' nend=', nend, ' np=', np

!added
write(*,*) 'input name of output file'
read(*,*) fout   !added e.g. E_0.2.dat   (E_dt.dat)
nfout=len_trim(fout) ! added nfout : length of fout (character file)
open(1,file=fout(1:nfout))  ! open 1 output file
write(1,'(a,a)') '# ',fout(1:nfout)
write(1,*) '# ','dt=',dt,'  nend=',nend,'  np=',np
write(1,*) '# t, u, exp(t), u-exp(t)'

u = 1.0
do n = 1, nend
    t  = dt*real(n)
    p1 = u
    p2 = u + 0.5d0*dt*p1
    p3 = u + 0.5d0*dt*p2
    p4 = u + dt*p3
    u  = u + dt*(p1 + 2*p2 + 2*p3 + p4)/6.0d0
    if (mod(n,np)==0) write(*,'(f7.3,3d15.7)') t, u, exp(t), u-exp(t)
    if (mod(n,np)==0) write(1,'(f7.3,3d15.7)') t, u, exp(t), u-exp(t)
end do 

end
