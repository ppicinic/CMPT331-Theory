PROGRAM ceaser
IMPLICIT NONE
character(len=51) :: test = "This is a test from alan testing testing testing @!"
call encrypt(test, 5)
call decrypt(test, 2)
call solve(test, 52)
!PRINT *,test

contains
 

subroutine toupper(x)
character(len=*) :: x
integer :: i
do i=1,len(x)
  if(x(i:i) >= "a" .and. x(i:i) <="z") then
    x(i:i) = achar(iachar(x(i:i)) - 32)
  else
    x(i:i) = x(i:i)
  endif
end do
end subroutine toupper

subroutine encrypt(str, shiftAmount)
character(len=*) :: str
integer :: shift, shiftAmount
character(len=len(str)) :: result
integer :: i
result = str
call toupper(result)
shift = mod(shiftAmount, 26)
do i=1,len(str)
  if(result(i:i) >= "A" .and. result(i:i) <= "Z") then
    result(i:i)= achar(iachar(result(i:i)) + shift)
    if(result(i:i) > "Z") then
      result(i:i) = achar(iachar("A") + (iachar(result(i:i)) - iachar("Z") - 1))
    endif
  endif
end do
print *,result
end subroutine encrypt

subroutine decrypt(str, shiftAmount)
character(len=*) :: str
integer :: shift, i, shiftAmount
character(len=len(str)) :: result
result = str
call toupper(result)
shift = mod(shiftAmount, 26)
do i=1,len(str)
  if(result(i:i) >= "A" .and. result(i:i) <= "Z") then
    result(i:i)=achar(iachar(result(i:i)) - shift)
    if(result(i:i) < "A") then
      result(i:i) = achar(iachar("Z") - (iachar("A") - iachar(result(i:i)) - 1))
    endif
  endif
end do
print *,result
end subroutine decrypt

subroutine solve(str, maxShiftAmount)
character(len=*) :: str
integer :: i, maxShiftAmount
do i=maxShiftAmount,0,-1
  write (*,"(A)",advance="no") "CEASER "
  write (*,"(I2)",advance="no") i
  call decrypt(str, i)
end do
end subroutine solve

end program ceaser