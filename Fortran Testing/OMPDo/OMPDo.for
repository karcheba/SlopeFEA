      PROGRAM OMPDo

!$    USE OMP_lib

      INTEGER :: i

!$OMP PARALLEL
!$OMP DO
      DO i = 1,15
      PRINT*, "ITERATION", i
!$    PRINT*, "ON THREAD",
!$   *         OMP_get_thread_num()+1, " OF", OMP_get_num_threads()
      END DO
!$OMP END DO
!$OMP END PARALLEL

      END PROGRAM OMPDo