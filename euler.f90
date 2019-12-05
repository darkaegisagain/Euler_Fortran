
! primgen library
module primgen
  use iso_c_binding
  interface
     ! int gen_prime_array(int array_size, int64 *array, int64 limit)
     function  gen_prime_array(array_size, array, limit) bind(C, name="gen_prime_array")
       use, intrinsic :: iso_c_binding
       integer(c_int) :: gen_prime_array
       integer(c_int) :: array_size
       integer(8), dimension(*) :: array
       integer(8) :: limit
     end function gen_prime_array
  end interface
end module primgen

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
  integer :: count, start
  integer(8) :: array_of_primes(:)
end subroutine find_primes



subroutine problem_3
  use primgen
  use iso_c_binding
  integer(8) :: number = 600851475143
  integer(8), dimension(:) :: prime_array(:)
  integer(c_int) :: array_size = 10000
  integer(c_int) :: prime_count

  allocate(prime_array(array_size))

  prime_count = gen_prime_array(array_size, array_of_primes(array_size), number)

  print *, "generated ", count, " primes"

  deallocate(prime_array)
end subroutine problem_3



program main
  !call problem_1
  !call problem_2
  call problem_3
end program main
