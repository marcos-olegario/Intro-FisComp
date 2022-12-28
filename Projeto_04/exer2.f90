PROGRAM exer2
IMPLICIT NONE

    ! Variáveis de entrada
    REAL(8) :: dt, lbd
    INTEGER :: N0

    ! Tempo inicial e final da simulação
    REAL(8) :: t0 = 0.0d0, t = 10.0d0

    ! Tempo médio exato e calculado:
    REAL(8) :: t_med_exato, t_med_calc
 
    ! Número de iterações e iteradores:
    INTEGER :: num, i, j

    ! Átomos restantes
    INTEGER :: aux, atomos_restantes, atomos_decaidos = 0

    ! Abrindo o arquivo
    OPEN(UNIT=8, FILE='decai_out', ACTION='write', STATUS='replace')

    READ(*,*) N0
    READ(*,*) dt
    READ(*,*) lbd

    ! Número de iterações:
    num = INT((t - t0)/dt)

! -------------------------------------------------------------
    ! Solução analítica:

    ! Número de átomos no instante t=0
    !WRITE(8,*) 0.0d0, N0

    !DO i=1, num, 1

        ! Solução analítica
    !    atomos_restantes = INT(N0 * EXP(-lbd * i*dt))

        ! Escrevendo no arquivo
    !    WRITE(8,*)  i*dt, atomos_restantes

    !END DO
! -----------------------------------------------------------

    ! Imprimindo o tempo médio:

    t_med_exato = 1.0d0/lbd

    ! Condições iniciais
    t_med_calc = 0.0d0
    atomos_restantes = N0
    atomos_decaidos = 0

    DO i=1, num, 1

        aux = atomos_decaidos

        DO j=1, (atomos_restantes), 1

            IF (rand() < lbd * dt) THEN
                atomos_decaidos = atomos_decaidos + 1
            END IF
        
        END DO

        atomos_restantes = N0 - atomos_decaidos

        ! Tempo de vida médio:
        t_med_calc = t_med_calc + (i*dt * (atomos_decaidos - aux))/(N0)

        ! Escrevendo no arquivo
        WRITE(8,*)  i*dt, atomos_restantes

    END DO

    ! Condição para que não restem atomos ao final da simulação:
    ! t_med_calc = t_med_calc + (10.0d0 * (atomos_decaidos))/N0

    PRINT *, t_med_calc, t_med_exato

END PROGRAM exer2