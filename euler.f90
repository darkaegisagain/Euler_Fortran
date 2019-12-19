! ****************************************************
! PROBLEM 1
! ****************************************************
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

! ****************************************************
! PROBLEM 2
! ****************************************************
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

! ****************************************************
! PROBLEM 3
! ****************************************************
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


! ****************************************************
! PROBLEM 4
! ****************************************************
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

  
! ****************************************************
! PROBLEM 5
! ****************************************************
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
  
! ****************************************************
! PROBLEM 6
! ****************************************************
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

! ****************************************************
! PROBLEM 7
! ****************************************************
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


! ****************************************************
! PROBLEM 8
! ****************************************************
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

! ****************************************************
! PROBLEM 9
! ****************************************************
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

! ****************************************************
! PROBLEM 10
! ****************************************************
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


! ****************************************************
! PROBLEM 11
! ****************************************************
subroutine problem_11
  implicit none
  integer :: grid(1:20, 1:20)
  integer :: product(4), max_value
  integer :: vert(1:17,1:20), horz(1:20,1:17), diag1(1:17,1:17), diag2(1:17,1:17)
  integer :: i, j
  
  open(unit=1, file="problem_11.txt")
  read(1, *) ((grid(i, j), i=1,20), j=1,20)
  close(1)

  print *,"grid"
  write(*,1) ((grid(i,j), i=1,20), j=1,20)
1 format(I3,I3,I3,I3,I3,I3,I3,I3,I3,I3,I3,I3,I3,I3,I3,I3,I3,I3,I3,I3)

  vert = grid(1:17,1:20) * grid(2:18,1:20) * grid(3:19,1:20) * grid(4:20,1:20)
  horz = grid(1:20,1:17) * grid(1:20,2:18) * grid(1:20,3:19) * grid(1:20,4:20)
  diag1 = grid(1:17,1:17) * grid(2:18,2:18) * grid(3:19,3:19) * grid(4:20,4:20)
  diag2 = grid(4:20,1:17) * grid(3:19,2:18) * grid(2:18,3:19) * grid(1:17,4:20)

  max_value = max(maxval(vert), maxval(horz), maxval(diag1), maxval(diag2))

  print *, max_value
end subroutine problem_11



! ****************************************************
! PROBLEM 12
! ****************************************************
integer function num_divisors(N)
  implicit none
  integer(8) :: N
  integer(8) :: count, num_div, temp, two, P

  two = 2
  if (mod(N, two) == 0) then
     N = N / 2
  end if

  num_div = 1
  
  count = 0
  count_evens : do
     if (mod(N, two) /= 0) exit

     count = count + 1

     N = N / 2
  end do count_evens
  num_div = num_div * (count + 1)


  P = 3
  count_odds : do
     if (N == 1) exit

     count = 0
     do
        if (mod(N, P) /= 0) exit

        count = count + 1

        N = N / P
     end do
     num_div = num_div * (count + 1)

     P = P + 2
  end do count_odds

  
  num_divisors = num_div  
end function num_divisors


integer(8) function triangle_number(i)
  implicit none
  integer :: i

  triangle_number = i * (i + 1) / 2
end function triangle_number

subroutine problem_12
  implicit none

  !function defs
  integer(8) :: triangle_number
  integer :: num_divisors

  !variables
  integer(8) :: i, n, np1
  
  i = 1
  n = num_divisors(i)
  np1 = num_divisors(i + 1)

  do
     if ((n * np1) > 500) then
        print *, "Triangle number", triangle_number(i), "index", i, "terms", n * np1
        exit
     end if

     print *,"n", n, "np1", np1, "index", i
     i = i + 1
     n = np1
     np1 = num_divisors(i+1)
  end do
  
end subroutine problem_12

! ****************************************************
! PROBLEM 13
! ****************************************************
subroutine problem_13
  implicit none
  integer :: i, j, pten
  character(50) :: inputline(1:100), tempstr
  integer :: ibuffer(1:100)
  integer :: t(1:5)
  integer :: stat
  integer(8) :: sum
  integer :: c
  
  open(unit=1, file="problem_13.txt")
  do i=1,100
     read(1, '(A)', iostat=stat) inputline(i)

     print *, inputline(i)
  end do
  close(1)

  sum = 0
  pten = 1
  do i=1,10
     do j=1,100
        tempstr = inputline(j)
        c = ichar(tempstr(i:i)) - 48
        sum = sum + c * pten
     end do
     pten = pten * 10
  end do

  print *,"sum", sum
end subroutine problem_13

! ****************************************************
! PROBLEM 14
! ****************************************************
subroutine problem_14
  implicit none
  integer(8) :: N, two
  integer :: i, oneM, n_terms, max_terms, max

  max = 0
  max_terms = 0
  oneM = 1e6
  two = 2
  
  do i=1,oneM
     N = i
     
     if (mod(N,two) == 0) then
        N = N / 2
     else
        N = 3 * N + 1
     end if

     print *, "i", i, "N", N
     
     n_terms = 1
     
     do
        if (mod(N,two) == 0) then
           N = N / 2
        else
           N = 3 * N + 1
        end if

        n_terms = n_terms + 1
        
        if (n_terms > max_terms) then
           max_terms = n_terms
           max = i
        end if
        
        if (N == 1) exit
     end do
  end do

  print *, "Max terms", max_terms, "i", max
end subroutine problem_14

! ****************************************************
! PROBLEM 15
! ****************************************************
subroutine problem_15
  implicit none

  integer(8) :: grid(1:20,1:20), sum
  integer(4) :: i, j, gs

  do gs = 2,21
     ! clear grid
     do i=1,gs
        do j=1,gs
           grid(i,j) = 0
        end do
     end do

     ! fill in top left egdges with 1s for 1 possible way to get there
     do i=1,gs
        grid(i,1) = 1
        grid(1,i) = 1
     end do

     ! calc number of paths
     do i=2,gs
        do j=2,gs
           grid(i, j) = grid(i,j-1) + grid(i-1,j)
        end do
     end do

     print *,"max paths", grid(gs,gs), "grid size",gs-1
  end do
  
end subroutine problem_15
   
  
! ****************************************************
! PROBLEM 16
! ****************************************************
subroutine problem_16
  implicit none
  integer :: sum(1:500)
  integer :: i,j,temp, carry

  !clear array
  do i=1,500
     sum(i) = 0
  end do

  
  !calc 2 ^ 1000
  sum(1) = 1
  
  do i=1,1000

     ! add a number to itself = times 2
     ! do this 1000 times
     temp = 0
     carry = 0
     do j=1,500
        temp = sum(j) + sum(j) + carry
        if (temp > 9) then
           sum(j) = mod(temp,10)
        else
           sum(j) = temp
        end if
        carry = temp / 10
     end do

  end do

  temp = 0
  do i=1,500
     !print *,"sum",sum(i)
     temp = temp + sum(i)
  end do

  print *, "sum", temp
  
end subroutine problem_16

! ****************************************************
! PROBLEM 17
! ****************************************************
subroutine problem_17
  implicit none
  integer :: fullNumToStrLen
  integer :: num, sum, i, j
  character(len=30) :: thousands(1:10)
  character(len=30) :: hundreds(1:10)
  character(len=30) :: tens(1:10)
  character(len=30) :: teens(1:10)
  character(len=30) :: ones(1:10)
  character(len=200) :: fullnumber
  character :: c
  integer :: t(1:4), p10, temp
  
  thousands(1) = 'one thousand'
  
  hundreds(1) = 'one hundred'
  hundreds(2) = 'two hundred'
  hundreds(3) = 'three hundred'
  hundreds(4) = 'four hundred'
  hundreds(5) = 'five hundred'
  hundreds(6) = 'six hundred'
  hundreds(7) = 'seven hundred'
  hundreds(8) = 'eight hundred'
  hundreds(9) = 'nine hundred'
  
  tens(1) = 'ten'
  tens(2) = 'twenty'
  tens(3) = 'thirty'
  tens(4) = 'forty'
  tens(5) = 'fifty'
  tens(6) = 'sixty'
  tens(7) = 'seventy'
  tens(8) = 'eighty'
  tens(9) = 'ninety'

  teens(1) = 'eleven'
  teens(2) = 'twelve'
  teens(3) = 'thirteen'
  teens(4) = 'fourteen'
  teens(5) = 'fifteen'
  teens(6) = 'sixteen'
  teens(7) = 'seventeen'
  teens(8) = 'eighteen'
  teens(9) = 'nineteen'

  ones(1) = 'one'
  ones(2) = 'two'
  ones(3) = 'three'
  ones(4) = 'four'
  ones(5) = 'five'
  ones(6) = 'six'
  ones(7) = 'seven'
  ones(8) = 'eight'
  ones(9) = 'nine'

  sum = 0
  do num=1,1000

     p10 = 1000
     temp = num
     do i=0,3
        t(4-i) = temp / p10
        temp = temp - (temp / p10) * p10
        p10 = p10 / 10
     end do

     !print *,t(4),t(3),t(2),t(1), i
     fullnumber = trim('')

     ! thousand
     if (t(4) > 0) then
        fullnumber = trim(fullnumber)//thousands(1)
     end if
 
     !print *, fullnumber

     ! hundreds
     if (t(3) > 0) then
        if (t(4) > 0) then
           fullnumber = trim(fullnumber)//' '//hundreds(t(3))
        else
           fullnumber = hundreds(t(3))
        end if
     end if

     print *, fullnumber

     ! insert 'and'
     if (t(4) > 0 .or. t(3) > 0) then
        if (t(2) > 0 .or. t(1) > 0) then
           ! one thousand five hundred and
           fullnumber = trim(fullnumber)//' and '
        end if
     end if

     ! teens
     if (t(2) == 1 .and. t(1) > 0) then
        fullnumber = trim(fullnumber)//' '//teens(t(1))
     else
        if (t(2) > 0) then
           fullnumber = trim(fullnumber)//' '//tens(t(2))
        end if
        
        if (t(1) > 0) then
           fullnumber = trim(fullnumber)//' '//ones(t(1))
        end if
     end if

     print *, fullnumber

     fullnumber = trim(fullnumber)

     temp = 0
     do j=1,len(fullnumber)
        c = fullnumber(j:j)
        
        if (c >= 'a' .and. c <= 'z') then
           temp = temp + 1
        end if
     end do

     if (num == 342 .or. num == 115) then
        print *, fullnumber, temp
     end if
     
     sum = sum + temp
  end do

  print *, sum
  
end subroutine problem_17

! ****************************************************
! PROBLEM 18
! ****************************************************
subroutine problem_18
  implicit none
  integer :: triangle(1:15,1:15)
  integer :: triangle_path(1:15,1:15)
  integer :: i, j, sums(1:14,1:14)
  integer :: stat
  
  open(unit=1, file="problem_18.txt")

  do i=1,15
     read(1, *,iostat=stat) triangle(i,1:i)
  end do
    
  close(1)

  do i=14,1,-1
     do j=1, i
        ! go from bottom up summing

        triangle(i,j) = triangle(i,j) + max(triangle(i+1,j), triangle(i+1,j+1))
     end do
  end do

  print *, triangle(1,1)
  
end subroutine problem_18

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
  !call problem_11
  !call problem_12
  !call problem_13  it looks like my changes didn't make it in...
  !call problem_14
  !call problem_15
  !call problem_16
  !call problem_17

  call problem_18
  
end program main
