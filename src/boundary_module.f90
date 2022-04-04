!> @brief This module is used to set the boundary conditions.
!> @details The module contains the soubrutines for different
!! boundary conditions, both internal and external.
!! Internal boundary conditions are considered only in the
!! case of parallel computation via domain decomposition.
module boundary
  use data_type
  use mpi
implicit none

contains

  !> @brief This subroutine perform a 2D boundary exchange.
  !> @details The subroutine perform boundary exhange along the
  !! West-East direction and North-South. Input parameters are the
  !! boundary to send along the 4 side of the submesh and the
  !! mesh, simParam and boundary data structures.
  subroutine mpiExchange(sendE,sendN,sendW,sendS,sPar,msh,bndry)

    type(mesh)                       :: msh !< Mesh data structue.
    type(simParam)                   :: sPar!< simParam data structue.
    type(bndryCondition)             :: bndry!< boundaryCondition data structue.
    integer                          :: tagWE,tagSN,tagEW,tagNS !> West,North,East,South tags.
    integer                          :: ierr !< MPI error integer
    integer                          :: status(mpi_status_size) !< MPI status integer
    real*8, dimension(msh%nLocal(1)) :: sendE, sendW !< Boundary to send of length nLocal(1) (number of local rows)
    real*8, dimension(msh%nLocal(2)) :: sendS, sendN !< Boundary to send of length nLocal(2) (number of local columns)


    tagWE = 10 ! tags for mutual sendrecv
    tagEW = 20
    tagNS = 30
    tagSN = 40

    !Send West bndry values to left and recive East bndry values from right
    call mpi_sendrecv(sendW,msh%nLocal(1),mpi_double_precision,msh%idW,tagWE,bndry%intBndryE,&
                      msh%nLocal(1),mpi_double_precision,msh%idE,tagWE,msh%cartComm,status,ierr)
    !Send East bndry values to right and recive West bndry values from left
    call mpi_sendrecv(sendE,msh%nLocal(1),mpi_double_precision,msh%idE,tagEW,bndry%intBndryW,&
                      msh%nLocal(1),mpi_double_precision,msh%idW,tagEW,msh%cartComm,status,ierr)
    !Send North bndry values to up and recive South bndry values from Down
    call mpi_sendrecv(sendN,msh%nLocal(2),mpi_double_precision,msh%idN,tagNS,bndry%intBndryS,&
                      msh%nLocal(2),mpi_double_precision,msh%idS,tagNS,msh%cartComm,status,ierr)
    !Send South bndry values to down and recive North bndry values from Up
    call mpi_sendrecv(sendS,msh%nLocal(2),mpi_double_precision,msh%idS,tagSN,bndry%intBndryN,&
                      msh%nLocal(2),mpi_double_precision,msh%idN,tagSN,msh%cartComm,status,ierr)


  end subroutine mpiExchange


end module boundary
