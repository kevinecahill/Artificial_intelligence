program doesitwork
  implicit none
  integer(4)::i, j, k, label, glabel
  integer(4),dimension(10000)::guesses
  integer(4),dimension(10000)::testilabels
  real(8)::max, sam, grade
  real(8),dimension(784,0:9)::thetenvectors
  real(8),dimension(10000,784)::tenktestvectors
  real(8),parameter::zero=0.0d0, eps=1.0d-4
  open(1,file="thetenvectors")
  do label = 0, 9
     do j = 1, 784
        read(1,*) thetenvectors(j,label)
     end do
  end do
  close(1)
  open(2,file="tenktestvectors")
  do i = 1, 10000
     do j = 1, 784
        read(2,*) tenktestvectors(i,j)
     end do
  end do
  close(2)
  open(3,file="testilabels")
  do k = 1, 10000
     read(3,*) testilabels(k)
  end do
  close(3)
  ! guess
  do i = 1, 10000
     max = zero; glabel = -1
     do j = 0, 9
        sam = dot_product( thetenvectors(:,j), &
             tenktestvectors(i,:) )
        if (sam > max) then
           max = sam; glabel = j
        end if
     end do
     guesses(i) = glabel
  end do
  ! grade
  grade = zero 
  do  i = 1, 10000
     if ( guesses(i) == testilabels(i) ) then
        grade = grade + eps
     end if
  end do
  print *, grade
end program doesitwork
