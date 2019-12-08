
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


subroutine problem_4
  implicit none
  integer :: palindromic_numbers(1:1000)
  integer :: num_palindromic_numbers
  integer :: ten_2, ten_1, ten_0
  integer :: temp, i, j, k
  integer :: max_num
  
  i = 1
  do ten_2 = 0,9
     do ten_1 = 0,9
        do ten_0 =  1,9

           temp = ten_0 * 1

           temp = temp + ten_1 * 10

           temp = temp + ten_2 * 100

           temp = temp + ten_2 * 1000

           temp = temp + ten_1 * 10000

           temp = temp + ten_0 * 100000
           
           !print *, "Palindronic number: ", i, " = ", temp
           palindromic_numbers(i) = temp

           i = i + 1
        end do
     end do
  end do

  num_palindromic_numbers = i

  max_num = 0

  do i = 0, 999
     do j = 0, 999

        if (j == i) then
           exit
        end if
        
        temp = i * j

        do k = 1, num_palindromic_numbers

           if (temp == palindromic_numbers(k)) then

              if (temp > max_num) then
                 max_num = temp
                 print *, "max = ", temp, "i = ", i, "j = ", j
              end if

           end if

        end do
     
     end do

  end do
end subroutine problem_4

  
subroutine problem_5
  integer :: max
  integer :: i
  logical :: is_ed

  max = 20
  do
     is_ed = .true.

     do i = 2,20
        if (mod(max, i) > 0) then
           is_ed = .false.
           exit
        end if
     end do

     if (is_ed) then
        print *, "result = ", max
        exit
     end if

     max = max + 1
  end do
end subroutine problem_5
  
subroutine problem_6
  integer :: sum_of_squares
  integer :: sum, square_of_sum
  integer :: diff
  integer :: i

  sum_of_squares = 0
  sum = 0
  
  do i = 1, 100
     sum_of_squares = sum_of_squares + i * i
     sum = sum + i
  end do

  square_of_sum = sum * sum

  diff = square_of_sum - sum_of_squares

  print *, "sum of squares = ", sum_of_squares
  print *, "sum = ", sum, "sum squared = ", square_of_sum
  print *, "diff = ", diff  
end subroutine problem_6

subroutine problem_7
  implicit none
  integer(8) :: at, primes(1:10001)
  integer :: i, j
  logical :: is_prime
  
  at = 3
  primes(1) = 2

  fill_primes : do i = 2, 10001
     gen_primes : do
        ! assume its prime
        is_prime = .true.

        check_is_prime :do j = 1, i - 1
           if (mod(at, primes(j)) == 0) then
              is_prime = .false.
              exit
           end if
        end do check_is_prime

        if (is_prime) then
           print *, "found prime # ", at, "index ", i
           primes(i) = at
           exit
        end if

        at = at + 1
     end do gen_primes
  end do fill_primes
end subroutine problem_7

subroutine problem_8
  implicit none
  character(len = 1000) :: bignum
  integer(8) :: i, j, index, strlen, max, temp, c
  
  bignum = "73167176531330624919225119674426574742355349194934&
  &96983520312774506326239578318016984801869478851843&
  &85861560789112949495459501737958331952853208805511&
  &12540698747158523863050715693290963295227443043557&
  &66896648950445244523161731856403098711121722383113&
  &62229893423380308135336276614282806444486645238749&
  &30358907296290491560440772390713810515859307960866&
  &70172427121883998797908792274921901699720888093776&
  &65727333001053367881220235421809751254540594752243&
  &52584907711670556013604839586446706324415722155397&
  &53697817977846174064955149290862569321978468622482&
  &83972241375657056057490261407972968652414535100474&
  &82166370484403199890008895243450658541227588666881&
  &16427171479924442928230863465674813919123162824586&
  &17866458359124566529476545682848912883142607690042&
  &24219022671055626321111109370544217506941658960408&
  &07198403850962455444362981230987879927244284909188&
  &84580156166097919133875499200524063689912560717606&
  &05886116467109405077541002256983155200055935729725&
  &71636269561882670428252483600823257530420752963450"

  strlen = len(bignum)

  print *, "strlen = ", strlen, "bignum ", bignum

  max = 0
  
  do i = 1, (1000 - 13)
     temp = 1
     
     do j = 0, 12
        if (j + i > 1000) then
           stop
        end if

        index = i + j
        c = ichar(bignum(index:index)) - 48

        if (c == 0) then
           temp = 1
           exit
        end if
        
        temp = temp * c
     end do      
     
     if (temp > max) then
        max = temp
        
        print *, "max value = ", max, "chars = ", bignum(i:i+12)
     end if
  end do

  print *, "max value ", max
end subroutine problem_8

subroutine problem_9
  integer :: a, b, c, triplet
  integer :: as, bs, cs, as_p_bs, product
  
  do a = 1, 1000
     as = a * a
     
     do b = 1, a
        bs = b * b

        as_p_bs = as + bs

        if ((a + b) > 1000) then
           exit
        end if
        
        do c = 1, 1000 
           cs = c * c

           triplet = a + b + c

           if (triplet > 1000) then
              exit
           end if
           
           if (triplet == 1000) then
              if (as_p_bs == cs) then
                 product = a * b * c
                 
                 print *, "product ", product, "a", a, "b", b, "c", c

                 return
              end if
           end if
        end do
     end do
  end do
end subroutine problem_9

#define P_NUM_PRIMES 1000000
subroutine problem_10
  implicit none
  integer :: num_primes = P_NUM_PRIMES
  integer(8) :: at, primes(1:P_NUM_PRIMES)
  integer(8) :: max_prime = 2000000
  integer(8) :: sum = 0
  integer :: i, j
  logical :: is_prime
  
  at = 3
  primes(1) = 2

  fill_primes : do i = 2, num_primes
     gen_primes : do
        ! assume its prime
        is_prime = .true.

        check_is_prime :do j = 1, i - 1
           if (mod(at, primes(j)) == 0) then
              is_prime = .false.
              exit
           end if
        end do check_is_prime

        if (is_prime) then
           !print *, "found prime # ", at, "index ", i
           primes(i) = at
           exit
        end if

        at = at + 1
     end do gen_primes

     if (at > max_prime) then
        exit
     end if
  end do fill_primes

  sum = 0
  do i = 1, num_primes
     if (primes(i) > max_prime) then
        exit
     end if

     print *, "prime ", i, primes(i)
     
     sum = sum + primes(i)

     print *, "sum", sum
  end do

  print *, "sum", sum
  
end subroutine problem_10

subroutine problem_11
  implicit none
  integer :: grid(1:20, 1:20)
  integer :: row, col, i, j, k, ik, jk
  integer(8) :: product(4), max
  integer :: terms(1:4, 0:3)
  
  open(unit=1, file="problem_11.txt")
  read(1, *) ((grid(i, j), i=1,20), j=1,20)
  close(1)

  write(*,1) ((grid(i,j), i=1,20), j=1,20)
1 format(I3,I3,I3,I3,I3,I3,I3,I3,I3,I3,I3,I3,I3,I3,I3,I3,I3,I3,I3,I3)
  max = 0

  do_row : do row=1,20
     do_col : do col=1,20
        
        do_clear : do k=1,4
           product(k) = 1
        end do do_clear

        do_products : do k=0,3
           i = row
           j = col
           
           ik = row + k
           jk = col + k

           ! mult to right
           if (jk <= 20) then
              product(1) = product(1) * grid(jk, i)
              terms(1,k) = grid(jk, i)
           else
              product(1) = 0
           endif

           ! mult down
           if (ik <= 20) then
              product(2) = product(2) * grid(j, ik)
              terms(2,k) = grid(j, ik)
           else
              product(2) = 0
           end if

           ! mult diagonally
           if ((ik <= 20) .and. (jk <= 20)) then
              product(3) = product(3) * grid(jk, ik)
              terms(3,k) = grid(jk, ik)
           else
              product(3) = 0
           end if

           ! mult diagonally
           ik = row - k
           jk = col
           if ((ik >= 1) .and. (jk <= 20)) then
              product(4) = product(4) * grid(jk, ik)
              terms(4,k) = grid(jk, ik)
           else
              product(4) = 0
           end if
        end do do_products

        do_check_max : do k = 1,4
           if (product(k) > max) then
              max = product(k)
           end if
        end do do_check_max
     end do do_col
  end do do_row

  print *, "max ", max
end subroutine problem_11

program main
  implicit none

  !call problem_1
  !call problem_2
  !call problem_3
  !call problem_4
  !call problem_5
  !call problem_6
  !call problem_7
  !call problem_8
  !call problem_9
  !call problem_10

  call problem_11
  
end program main
