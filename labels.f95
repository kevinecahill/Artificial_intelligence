program labels
  implicit none
  integer(4)::i
  integer(4)::label
  real(8)::dlabel
  open(7,file="ilabels")
  open(2,file="labels.txt")
  do i = 1, 60000
     read(2,*) dlabel
     label = dlabel
     write(7,*) label
  end do
  close(2)
  close(7)
end program labels
