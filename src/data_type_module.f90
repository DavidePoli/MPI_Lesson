!> @brief This module contains the datatypes used
module data_type
  !> @brief Data strucuture containing the mesh parameters
  !> @details
  !! In this data strucutre are contained all the parameters
  !! regarding the mesh, including centroid coordinates,
  !! centroid spacing and cell dimensions.
  !! The datatype considers a 2D mesh
  type mesh
    integer         :: nCell(2) !< Number of cells (y,z).
    integer         :: nXm      !< Product of matrix dimensions.
    real*8          :: domain(2) !< Domain size (y,z).
    real*8, pointer :: dZ(:,:) !< Distance between ij cell and ij+1 centroid.
    real*8, pointer :: dY(:,:) !< Distance between ij cell and i+1j centroid.
    real*8, pointer :: w(:,:) !< Cell width for ij cell.
    real*4, pointer :: h(:,:) !< Cell heigth for ij cell.
    real*8, pointer :: zC(:,:) !< z oordinate for ij cell centroid.
    real*8, pointer :: yC(:,:) !< y coordinate for ij cell centroid.
    integer         :: cartComm !< Cartesian topology communicator.
    integer         :: procDim(2) !< Number of processor for each dimension.(Z,Y) ATTENTION
    integer         :: mshId !< rank of process in sub mesh.
    integer         :: idCoord(2) !< Coordinates of mpi process.
    integer         :: idW, idE, idN, idS !< West, East, North, South neighbour process id.
    integer         :: nLocal(2) !< Local number of cells (y,z) one each process.
  end type mesh

  !> @brief Data strucuture containing the simulation parameters
  !> @details
  !! In this data strucutre are contained all the parameters
  !! regarding the simulation, flags, integration time ecc.
  type simParam
    integer           :: nCell(2) !< Number of cells (y,z)
    integer           :: iterations !< Number of time step
    real*4            :: dt !< Time step
    real*4            :: time(2) !< Start and End integration time
    integer           :: id !< Process id.
    integer           :: nProc !< Number of processes.
  end type simParam

  !> @brief Data structure containing the control flags
  !> @details This data structure contains the control flags for the code,
  !! including test flags, solver flags ecc. The flags are grouped in a
  !! integer vector.
  !! int(1) controls the test/program to be solved. 0 no test, 1 heat explicit, 2 heat implicit.
  !! int(2) controls the plotting of the result. 0 no , 1 yes.
  type flags
  integer :: int(2) !< Control integer.
  end type flags

  !> @brief Data structure containing test parameters and results.
  type test
  real*8, pointer    :: result(:,:) !< Test result.
  real*8, pointer    :: resultEvolution(:,:,:) !< Test result temporal evolution.
  character(len=255) :: testFile
  end type test

  !> @brief Data structure containing the boundary conditions information
  type bndryCondition
  real*8,pointer     :: NorthGhostCells(:,:)
  real*8,pointer     :: SouthGhostCells(:,:)
  real*8,pointer     :: EastGhostCells(:,:)
  real*8,pointer     :: WestGhostCells(:,:)
  real*8,pointer     :: intBndryN(:,:)
  real*8,pointer     :: intBndryE(:,:)
  real*8,pointer     :: intBndryS(:,:)
  real*8,pointer     :: intBndryW(:,:)
  integer            :: flag
  end type bndryCondition

end module data_type
