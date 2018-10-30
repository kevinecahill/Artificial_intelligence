program testvectors
  implicit none
  integer(4)::i, j, k
  real(8)::sum
  real(8), dimension(28)::line
  real(8),dimension(10000,784)::vectors09
  real(8),parameter::zero = 0.0d0
  ! make the ten vectors
  vectors09 = zero
  open(1,file="timagesreshaped.txt")
  do j = 1, 10000
     do i = 1, 28
        read(1,*) line
        do k = 1, 28
           vectors09(j,(i-1)*28 + k) = &
                vectors09(j,(i-1)*28 + k) + line(k)
        end do
     end do
  end do
  close(1)

  ! normalize the ten vectors
  do j = 1, 10000
     sum = zero
     do i = 1, 784
        sum = sum +  vectors09(j,i)**2
     end do
     sum = sqrt(sum)
     do i = 1, 784
        vectors09(j,i) = vectors09(j,i)/sum
     end do
  end do

  ! write them in a file
  open(8,file="tenktestvectors")
  do j = 1, 10000
     do i = 1, 784
        write(8,*) vectors09(j,i)
     end do
  end do
  close(8)
end program testvectors
