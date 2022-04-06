program communication
  use mpi
  use data_type
  use meshGen
  implicit none
  type(simParam)        :: sPar
  type(mesh)            :: msh
  integer :: ierr, tag1, tag2, Id, nproc, rcvId, sendId, rcv,rcvR, rcvL, bCast,i, flag, sumpart, sum
  integer, dimension(:), allocatable :: scatter, scatterPart, gather, gatherPart
  integer :: status(MPI_STATUS_SIZE)
  character(len=10) :: command

  if(command_argument_count().ne.1) then
    print*, 'ierr, command flag required'
    stop
  end if
  call get_command_argument(1,command)
  read(command,*) flag

  call mpi_init(ierr) !Initializr
  !get rank and nproc
  call mpi_comm_rank(mpi_comm_world,id,ierr)
  call mpi_comm_size(mpi_comm_world,nProc,ierr)
  if (flag==0) then
    !point to point communication
    if (Id==0) then
      print*, '********************************************'
      print*, ' point to point communication, easy but wrong'
      print*, '********************************************'
    end if
    call mpi_barrier(mpi_comm_world,ierr)
    tag1 = 195
    tag2 = 220
    if (Id == nproc-1) then
      sendId = 0
    else
      sendId = Id+1
    end if

    if (Id==0) then
      rcvId = nproc-1
    else
      rcvId = Id-1
    end if
    !send and receive
    call mpi_Send(Id, 1, mpi_int,sendId, tag1, mpi_comm_world, ierr)
    call mpi_Recv(rcv, 1, mpi_int, rcvId, tag1, mpi_comm_world, status, ierr)
    !print stuff
    print*, 'I am rank', Id, 'and I received',rcv,'from process',rcvId


  else if (flag==1) then
    !--------------------------------------------------------
    !collective communications
    call mpi_barrier(mpi_comm_world,ierr)
    if (Id==0) then
      print*, '********************************************'
      print*, ' Broadcast'
      print*, '********************************************'
    end if
    call mpi_barrier(mpi_comm_world,ierr)
    bCast = 0
    If (Id==0) then
      bCast = 50
    end if
    !now we call broadcast
    call mpi_bcast(bCast,1, mpi_int,0, mpi_comm_world, ierr)
    print*, bCast, Id
  else if (flag==2) then
    !--------------------------------------------------------
    ! Scatter
    call mpi_barrier(mpi_comm_world,ierr)
    if (Id==0) then
      print*, '********************************************'
      print*, ' Scatter'
      print*, '********************************************'
    end if
    if (Id==0) then
      allocate(scatter(nproc*4))
      scatter = [(i, i=1,nproc*4, 1)]
    end if
    allocate(scatterPart(4))
    call mpi_scatter(scatter,4,mpi_int,scatterPart,4, mpi_int,0,  mpi_comm_world,ierr)
    print*, 'I am ', Id,'I have', scatterPart


  else if (flag==3) then
    !--------------------------------------------------------
    ! gather
    call mpi_barrier(mpi_comm_world,ierr)
    if (Id==0) then
      print*, '********************************************'
      print*, ' Gather'
      print*, '********************************************'
    end if
    if (Id==0) then
      allocate(gather(nproc*4))
    end if
    allocate(gatherPart(4))
    gatherPart = [(i, i=1,4, 1)]
    call mpi_gather(gatherPart,4,mpi_int,gather,4, mpi_int, 0, mpi_comm_world,ierr)
    if (Id==0) then
      print*, 'I am rank',Id, 'I gathered', gather
    end if

  else if (flag==4) then
    !--------------------------------------------------------
    ! reduce
    call mpi_barrier(mpi_comm_world,ierr)
    if (Id==0) then
      print*, '********************************************'
      print*, ' Reduce'
      print*, '********************************************'
    end if
    sumpart = 100
    call mpi_reduce(sumpart, sum,1, mpi_int,mpi_sum, 0, mpi_comm_world,ierr )
    if (Id==0) then
      print*, 'the sum on rank', Id, 'is', sum
    end if


  else if (flag==5) then
    !--------------------------------------------------------
    ! cartshift
    msh%domain(1) = 12
    msh%domain(2) = 12
    msh%nCell(1) = 4
    msh%nCell(2) = 6
    sPar%id = Id
    sPar%nProc = nproc

    call testExchange(msh,sPar)

  else if (flag==6) then
    !point to point communication
    if (Id==0) then
      print*, '********************************************'
      print*, ' point to point communication both sides'
      print*, '********************************************'
    end if
    call mpi_barrier(mpi_comm_world,ierr)
    tag1 = 195
    tag2 = 220

    ! left boundary send
    if (id>0) then
      call MPI_Recv(rcvL, 1,mpi_int, id-1, tag1, mpi_comm_world, status, ierr )
    end if
    !right boundary send, right boundary receive
    if (id < nproc-1 ) then
      call MPI_sSend(id,1,mpi_int,id +1,tag1, mpi_comm_world, ierr )
      call MPI_Recv(rcvR, 1,mpi_int, id+1, tag2, mpi_comm_world, status, ierr )

    end if
    ! left boundary send
    if (id > 0) then
      call MPI_sSend(id,1,mpi_int,id -1,tag2, mpi_comm_world, ierr )
    end if
    print*, 'I am rank', Id, ' I received',rcvR,'from',id+1 , 'and',rcvL,'from',id-1

  else if (flag==7) then
    if (Id==0) then
      print*, '********************************************'
      print*, ' circular communication'
      print*, '********************************************'
    end if
    call mpi_barrier(mpi_comm_world,ierr)
    tag1 = 195
    tag2 = 220
    if (Id == nproc-1) then
      sendId = 0
    else
      sendId = Id+1
    end if

    if (Id==0) then
      rcvId = nproc-1
    else
      rcvId = Id-1
    end if

    if (Mod( Id, 2 )==0) then
      call MPI_Recv(rcv, 1,mpi_int, rcvId, tag1, mpi_comm_world, status, ierr )
      call MPI_sSend(id,1,mpi_int,rcvId,tag2, mpi_comm_world, ierr )

    else
      call MPI_sSend(id,1,mpi_int,sendId,tag1, mpi_comm_world, ierr )
      call MPI_Recv(rcv, 1,mpi_int, sendId, tag2, mpi_comm_world, status, ierr )
    end if

    print*, 'I am rank', Id, 'and I received',rcv,'from process',rcvId


  end if



  call mpi_finalize(ierr)





end program communication
