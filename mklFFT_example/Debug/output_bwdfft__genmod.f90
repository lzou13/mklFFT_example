        !COMPILER-GENERATED INTERFACE MODULE: Sun Oct 07 00:20:44 2018
        MODULE OUTPUT_BWDFFT__genmod
          INTERFACE 
            SUBROUTINE OUTPUT_BWDFFT(T,F,N)
              INTEGER(KIND=4), INTENT(IN) :: N
              REAL(KIND=4), INTENT(IN) :: T(N)
              COMPLEX(KIND=4) :: F(N/2+1)
            END SUBROUTINE OUTPUT_BWDFFT
          END INTERFACE 
        END MODULE OUTPUT_BWDFFT__genmod
