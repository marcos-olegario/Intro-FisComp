PROGRAM exer1
IMPLICIT NONE

    ! Variáveis de entrada
    REAL(8) :: dt, lbd
    INTEGER :: N0

    ! Tempo inicial e final da simulação
    REAL(8) :: t0 = 0.0d0, t = 10.0d0

    ! Número de iterações e iteradores:
    INTEGER :: num, i, j

    ! Átomos decaídos
    INTEGER :: atomos_restantes, atomos_decaidos = 0

    ! Abrindo o arquivo
    OPEN(UNIT=8, FILE='decai_out', ACTION='write', STATUS='new')

    READ(*,*) N0
    READ(*,*) dt
    READ(*,*) lbd

    ! Número de iterações:
    num = INT((t - t0)/dt)

    ! Número de átomos no instante t=0
    WRITE(8,*) 0.0d0, N0

    DO i=1, num, 1

        atomos_restantes = N0 - atomos_decaidos

        DO j=1, (atomos_restantes), 1

            IF (rand() < lbd * dt) THEN
                atomos_decaidos = atomos_decaidos + 1
            END IF
        
        END DO

        ! Escrevendo no arquivo
        WRITE(8,*)  i*dt, N0 - atomos_decaidos

    END DO

END PROGRAM exer1