program fibo
        implicit none
INTEGER*16  Fib(0:20000)
!double precision  Fib(0:20000)
!real*16  Fib(0:20000)
integer  FibMOD(0:20000)
integer :: n=100
integer j,MODf,i,countif
Fib(0)=0
Fib(1)=1
do i=2,n
Fib(i)=Fib(i-1)+Fib(i-2)
end do

do j=10,n-1

MODf=j


do i=0,10
!print *,Fib(i)
end do
!print *,"--"
do i=0,n
FibMOD(i) = mod( Fib(i), MODf)
end do
do i=60,70
!print *,i,Fib(i),FibMOD(i)
end do
!exit

do i=6,n
countif=i-5
if((FibMOD(i)==FibMOD(5)) .AND. (FibMOD(i-1)==FibMOD(4)) .AND.  FibMOD(i-2)==FibMOD(3) .AND. FibMOD(i-3)==FibMOD(2) &
        .AND.  FibMOD(i-4)==FibMOD(1) .AND. FibMOD(i-5)==FibMOD(0)) then
        exit
end if

end do

print *,MODf,countif,Fib(countif)
!print *,MODf,countif

end do
end
