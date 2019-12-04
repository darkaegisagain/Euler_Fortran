
! PROBLEM 1
subroutine problem_1
  integer :: i, count, sum

  count = 0
  sum = 0
  
  do i = 1, 999
     if ((mod(i, 3) == 0) .or. (mod(i, 5) == 0)) then
        count = count + 1
        sum = sum + i
        print *, i, "is a multiple of 3 or 5"
     end if
  end do

  print *, "Number of natural numbers that are multiples of 3 or 5 is ", count
  print *, "Sum ", sum
end subroutine problem_1

! PROBLEM 2
integer function fibonacci(x)
  implicit none
  integer :: x
  integer :: i
  integer :: x_m_1, x_m_2

  !print *, "fib ", x
  
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
        !print *, "fibonacci ", fibonacci, "x_m_1 ", x_m_1, "x_m_2 ", x_m_2
        if (fibonacci < 0) then
           stop
        end if
     end do
  end if

  print *, "fib ", x, " = ", fibonacci
  return
end function fibonacci

subroutine problem_2
  implicit none
  integer :: sum = 0
  integer :: even_sum = 0
  integer :: x = 0
  integer :: f
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

subroutine find_primes(count, start, array_of_primes)
  integer :: count, start, i
  long, pointer :: array_of_primes

  do i = (start, count)
     if (is_prime(i) then
end

subroutine problem_3
  long :: number = 600851475143
  
end subroutine problem_3

program main
  !call problem_1
  !call problem_2
  call problem_3
end program main


  
