program initialize
  use mpi
  implicit  none 
  integer :: id, nProc, ierr


  call mpi_init(ierr)

  call mpi_comm_rank(mpi_comm_world,id,ierr)
  call mpi_comm_size(mpi_comm_world,nProc,ierr)

  print*, 'I am process',id, 'out of',nProc

  call mpi_finalize(ierr)
end program initialize
