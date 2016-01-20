program Euler2
implicit none
real(8) :: dt, t,    u1, u2, utemp
integer :: n,  nend, np, nfout
character(130) :: fout

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

u1 = 1.0d0
u2 = (1 + dt)*u1

do n = 1, nend
    t     = dt*real(n)
    utemp = u2
    u2    = 2.0d0*dt*u1 + u2
    u1    = utemp
    if (mod(n,np)==0) write(*,'(f7.3,3d15.7)') t, u2, exp(t), u2-exp(t)
    if (mod(n,np)==0) write(1,'(f7.3,3d15.7)') t, u2, exp(t), u2-exp(t)
end do 

close(1)
end
