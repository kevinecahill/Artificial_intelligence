program tenvectors
  implicit none
  integer(4)::i, j, k, label
  real(8)::sum
  real(8), dimension(28)::line
  real(8),dimension(784,0:9)::vectors09
  real(8),dimension(784,0:9)::tenvectors
  real(8),parameter::zero = 0.0d0
  ! make the ten vectors
  vectors09 = zero
  open(1,file="imagesreshaped.txt")
  open(2,file="ilabels")
  do j = 1, 60000
     read(2,*) label
     do i = 1, 28
        read(1,*) line
        do k = 1, 28
           vectors09((i-1)*28 + k, label) = &
                vectors09((i-1)*28 + k, label) + line(k)
        end do
     end do
  end do
  close(2); close(1)

  ! normalize the ten vectors
  do label = 0, 9
     sum = zero
     do i = 1, 784
        sum = sum +  vectors09(i,label)**2
     end do
     sum = sqrt(sum)
     do j = 1, 784
        tenvectors(j,label) = vectors09(j,label)/sum
     end do
  end do

  ! write them in a file
  open(8,file="thetenvectors")
  do label = 0, 9
     do j = 1, 784
        write(8,*) thetenvectors(j,label)
     end do
  end do
  close(8)
end program tenvectors
