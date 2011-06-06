      PROGRAM SalaryIncrease
      
        IMPLICIT NONE
        
        INTEGER :: rank(9)
        REAL :: currSalaries(9), newSalaries(9), totalCost
        REAL :: tier1inc = 1.05, tier2inc = 1.04, tier3inc = 1.02
        
        rank = (/ 1,2,3,2,1,1,1,2,3 /)
        currSalaries = (/ 10500, 16140, 22300, 15960, 14150,
     *                     12180, 13230, 15760, 31000 /)
        newSalaries = currSalaries
        
        WHERE (rank .EQ. 1) newSalaries = newSalaries * tier1inc
        WHERE (rank .EQ. 2) newSalaries = newSalaries * tier2inc
        WHERE (rank .EQ. 3) newSalaries = newSalaries * tier3inc
        
        totalCost = SUM(newSalaries) - SUM(currSalaries)
        
        PRINT*, "Current Salaries:"
        PRINT*, currSalaries
        PRINT*
        PRINT*, "Hierarchy:"
        PRINT*, rank
        PRINT*
        PRINT*, "Tier 1, 2, 3, salary increase:"
        PRINT*, tier1inc, tier2inc, tier3inc
        PRINT*
        PRINT*, "New Salaries:"
        PRINT*, newSalaries
        PRINT*
        PRINT*, "Total Cost = ", totalCost
      
      END PROGRAM SalaryIncrease