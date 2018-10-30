program testlabels
  implicit none
  integer(4)::i
  integer(4)::label
  real(8)::dlabel
  open(7,file="testilabels")
  open(2,file="tlabels.txt")
  do i = 1, 10000
     read(2,*) dlabel
     label = dlabel
     write(7,*) label
  end do
  close(2)
  close(7)
end program testlabels
