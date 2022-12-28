PROGRAM exer3
IMPLICIT NONE

    ! Variáveis de entrada
    REAL(8) :: dt, lbd
    INTEGER :: N0

    ! Tempo inicial e final da simulação
    REAL(8) :: t0 = 0.0d0, T

    ! Número de iterações e iteradores:
    INTEGER :: num, i

    ! Átomos restantes
    INTEGER :: atomos_restantes

    ! Abrindo o arquivo
    OPEN(UNIT=8, FILE='decai_in', ACTION='read', STATUS='old')
    OPEN(UNIT=9, FILE='decai_out', ACTION='write', STATUS='replace')

    READ(8,*) T
    READ(8,*) N0
    READ(8,*) dt
    READ(8,*) lbd

    ! Número de átomos no instante t=0
    WRITE(9,*) 0.0d0, N0

    ! Número de iterações
    num = INT((t - t0)/dt)

    atomos_restantes = N0

    DO i=1, num, 1

        atomos_restantes = INT(atomos_restantes - atomos_restantes * lbd * dt)
        WRITE(9,*) i*dt, atomos_restantes

    END DO

END PROGRAM exer3
