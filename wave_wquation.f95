program Wave_equation
implicit none
integer, parameter:: im=11, im2=(im-1)/2
real(8), parameter:: L=1.0d0
real(8):: dt, t, dx, x, ctx, c, pi, u(-im:im,0:1)
integer:: i, n, nend, np,naf, nbf

write(*,*) 'input data, dt, c, nend, np'
read(*,*)  dt, c, nend, np
write(*,*) 'dt=',dt, ' c=',c, ' nend=', nend, ' np=',np

pi   = 4.0d0+atan(1.0d0)
dx   = 1.0d0/real(im-1)
ctx = c*dt/dx

!inital condition
u=0.0d0

do i= -im2, im2
    x      = pi*(dx*real(i) + 0.5d0*L)/L
    u(i,0) = sin(x)
end do
write(*,'(11f6.2)') (u(i,0), i=-im,im)

do n=1, nend
    t = dt*real(n)

    nbf = mod(n-1, 2)
    naf = mod(n,   2)

    do i= -(im-1), im-1
        u(i,naf) = u(i,nbf) -ctx*(u(i,nbf) - u(i-1,nbf))
    end do

    !periodic condition
    u(-im, naf) = u(-im+1, naf) 
    u(im,  naf) = u(im-1,  naf)

    if(mod(n,np)==0) then
        write(*,'(a7,f7.3)') 'time = ', t
        write(*,'(11f6.2)')  (u(i,naf),i=-im+1,im-1)
    endif
end do
end

