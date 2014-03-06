! Caesar Cypher - Fortran 95
! Phil Picinic
PROGRAM caesar
IMPLICIT NONE
character(len=35) :: en1 = "This is a test from alan testing @!"
character(len=21) :: en2 = "Check this code# !out"
character(len=35) :: de1 = "Estd tD L EpDE QCzX LWlY EPDEtyr @!"
character(len=21) :: de2 = "YdayG pdEO YkzA# !KqP"
character(len=35) :: so = "MABL bL T MxLM YkHF TETG MxLMBgZ @!" 
call encrypt(en1, 5)
call encrypt(en2, 16)
call decrypt(de1, 11)
call decrypt(de2, 22)
call solve(so, 26)

contains
 
! upper case subroutines, changes a string to upper case
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

! encrypt caesar cypher subroutine
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

! decrypt caesar cypher subroutine
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

! solve caesar cypher subroutine
subroutine solve(str, maxShiftAmount)
character(len=*) :: str
integer :: i, maxShiftAmount
do i=maxShiftAmount,0,-1
  write (*,"(A)",advance="no") "CEASER "
  write (*,"(I2)",advance="no") i
  call decrypt(str, i)
end do
end subroutine solve

end program caesar