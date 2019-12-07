
! PROBLEM 1
subroutine problem_1
  integer :: i, count, sum

  count = 0
  sum = 0

  do i = 1, 999
     if ((mod(i, 3) == 0) .or. (mod(i, 5) == 0)) then
        count = count + 1
        sum = sum + i
     end if
  end do
end subroutine problem_1

! PROBLEM 2
integer function fibonacci(x)
  implicit none
  integer :: x
  integer :: i
  integer :: x_m_1, x_m_2

  if (x == 0) then
     fibonacci = 1
  elseif (x == 1) then
     fibonacci = 2
  else
     x_m_1 = 2
     x_m_2 = 1
     fibonacci = 0

     do i = 2,x
        fibonacci = x_m_1 + x_m_2
        x_m_2 = x_m_1
        x_m_1 = fibonacci

        if (fibonacci < 0) then
           stop
        end if

     end do
  end if
end function fibonacci

subroutine problem_2
  implicit none
  integer :: sum = 0
  integer :: even_sum = 0
  integer :: x = 0
  integer :: f = 0
  integer :: fibonacci

  do while (f <= 4000000)
     f = fibonacci(x)
     !print *, "Fibonacci ", x, " = ", f

     sum = sum + f
     !print *, "sum = ", sum

     if (mod(f, 2) == 0) then
        even_sum = even_sum + f
     end if
     !print *, "even_sum = ", even_sum

     x = x + 1
  end do

  print *, "Sum is ", sum
  print *, "Even Sum is ", even_sum
end subroutine problem_2

subroutine problem_3
  use, intrinsic :: iso_c_binding
  implicit none

  integer(8) :: prime_limit = 600851475143
  integer(8) :: max_factor = 0, i;

  max_factor = prime_limit
  i = 2
  
  do
     if (mod(max_factor, i) == 0) then
        max_factor = max_factor / i
        print *, "found factor ", i, "reducing ", max_factor
        i = 2
     else
        i = i + 1
     end if

     if (i == max_factor) then
        exit
     end if
  end do

  print *, "max_factor = ", max_factor
end subroutine problem_3



program main
  implicit none

  !call problem_1
  !call problem_2
  call problem_3
  
end program main
