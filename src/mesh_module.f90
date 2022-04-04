!> @brief This module contains the subroutines to create the 2D mesh
module meshGen
  use data_type
  use mpi
  use boundary
  implicit none

contains
  !> @brief Subroutine to generate Cartesian Structured Uniform 2D mesh
  !> @details The subroutine creates a Cartesian Structured Uniform 2D mesh
  !! taking as input the mesh data structure. Before passing the data structure
  !! to the subroutine, the number of cell in (y,z) and the domain dimensions (y,z)
  !! must be specified in the data structure itself.
  !> @param[in] msh mesh data structure
  subroutine cartStructU2D(msh)
    type(mesh)     :: msh !< Mesh data structure
    integer        :: i

    allocate(msh%dZ(msh%nCell(1), msh%nCell(2)-1), msh%dY(msh%nCell(1)-1, msh%nCell(2)),&
              msh%h(msh%nCell(1), msh%nCell(2)), msh%w(msh%nCell(1), msh%nCell(2)),&
               msh%yC(msh%nCell(1), msh%nCell(2)), msh%zC(msh%nCell(1), msh%nCell(2)))

    msh%h = msh%domain(1)/msh%nCell(1) ! compute cell width
    msh%w = msh%domain(2)/msh%nCell(2) ! compute cell hiegth
    msh%yC(1,:) = msh%h(1,:)/2.
    msh%zC(:,1) = msh%w(:,1)/2.
    do i = 2, msh%nCell(1)
      msh%yC(i,:) = msh%yC(i-1,:) + (msh%h(i-1,:)+msh%h(i,:))/2. !compute cells centroid y coordinates
    end do
    do i = 2, msh%nCell(2)
      msh%zC(:,i) = msh%zC(:,i-1) + (msh%w(:,i-1)+msh%w(:,i))/2. !compute cells centroid z coordinates
    end do
    msh%dZ = (msh%zC(:,2:msh%nCell(2))-msh%zC(:,1:msh%nCell(2)-1)) !compute z centroid spacing
    msh%dY = (msh%yC(2:msh%nCell(1),:)-msh%yC(1:msh%nCell(1)-1,:)) !compute y centroid spacing
    msh%nXm = msh%nCell(2)*msh%nCell(1) !products of mesh dimensions
  end subroutine cartStructU2D

  !> @brief This subroutine performs the domain decomposition of the mesh.

  subroutine meshDomainDecomposition(msh,sPar)
    type(simParam) :: sPar !< simParam data structure
    type(mesh)     :: msh !< mesh data structure
    integer        :: ierr !< MPI error integer
    msh%procDim = 0 !Set no restrictions on proc dimensions
    call mpi_dims_create(sPar%nProc,2,msh%procDim,ierr) ! Recover number of proc per dimension
    ! Create communicator for cartesian decomposition
    call mpi_cart_create(mpi_comm_world,2,msh%procDim,[.False.,.False.],.TRUE.,msh%cartComm,ierr)
    call mpi_comm_rank(msh%cartComm,msh%mshId,ierr) !Assign rank to each process
    call mpi_cart_coords(msh%cartComm,msh%mshId,2,msh%idCoord,ierr) !Assign coordinates to each process
    call mpi_cart_shift(msh%cartComm,0,1,msh%idW, msh%idE, ierr) ! Assign id of neighbours on 0 dim
    call mpi_cart_shift(msh%cartComm,1,1,msh%idS, msh%idN, ierr) ! Assign id of neighbours on 0 dim
  end subroutine meshDomainDecomposition


  !> @brief Test subroutine for 2D mpi exchange
  subroutine testExchange(msh,sPar)
    type(mesh)                        :: msh
    type(simParam)                    :: sPar
    type(bndryCondition)              :: bndry
    real*8, allocatable, dimension(:) :: sendE, sendN, sendS, sendW

    call meshDomainDecomposition(msh,sPar)

    msh%nLocal(1) = msh%nCell(1)/msh%procDim(2)
    msh%nLocal(2) = msh%nCell(2)/msh%procDim(1)

    allocate(bndry%intBndryE(msh%nLocal(1),1))
    allocate(bndry%intBndryW(msh%nLocal(1),1))
    allocate(bndry%intBndryN(msh%nLocal(2),1))
    allocate(bndry%intBndryS(msh%nLocal(2),1))

    allocate(sendE(msh%nLocal(1)),sendW(msh%nLocal(1)))
    allocate(sendN(msh%nLocal(2)),sendS(msh%nLocal(2)))

    sendE = 1
    sendW = 5
    sendN = 3
    sendS = 8

    call mpiExchange(sendE,sendN,sendW,sendS,sPar,msh,bndry)
    print*, 'proc', msh%idCoord,'East bndry',bndry%intBndryE
    print*, 'proc', msh%idCoord,'West bndry',bndry%intBndryW
    print*, 'proc', msh%idCoord,'North bndry',bndry%intBndryN
    print*, 'proc', msh%idCoord,'South bndry',bndry%intBndryS

  end subroutine testExchange










end module meshGen
