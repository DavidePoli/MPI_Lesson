# MPI_Lesson

Install mpich
$ sudo apt-get install mpich

If you have also openMPI installed

$ update-alternatives --get-selections | grep mpi
$ update-alternatives --config mpi

select manual mode mpich


then go to the main folder and run make

to run the comm.out program run:

$ mpirun -np=n ./comm.out m

where n is the number of processes and m the desired example

examples:
0 = point to point communication
1 = Broadcast
2 = Scatter
3 = Gather
4 = Reduce
5 = CartShift

to run the initialization testcase
$ mpirun -np=n ./init.out 
