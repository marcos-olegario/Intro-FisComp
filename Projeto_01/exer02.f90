PROGRAM exer02
IMPLICIT NONE

    ! 1: Simples
    ! 2: Duplo

    ! Variáveis que armazenam os valores de x:

    REAL(4) :: x1
    REAL(8) :: x2

    ! Variáveis que armazenam o ultimo termo e a série:

    REAL(4) :: eps1, vp1
    REAL(8) :: eps2, vp2

    ! Arrays que armazenam as precisões para cada x:

    REAL(4), DIMENSION(4) :: precisao1
    REAL(8), DIMENSION(4) :: precisao2

    ! Variáveis que armazenam o fatorial de um número em precisão simples e dupla
    
    REAL(4) :: fatorial1
    REAL(8) :: fatorial2

    ! Contadores:

    INTEGER :: i, j, a, b, k

    ! Calculo da serie para 4 valores de x (0.1, 0.2, 0.3, 0.4) em precisão simples:

    DO a = 1, 4

        ! Inicializando variáveis

        x1 = 0.1e0 * a
        eps1 = 2.0e0 * EPSILON(1.0e0)
        vp1 = 0.0e0

        ! Inicializando contadores:
        
        i = 0

        DO WHILE(ABS(eps1) > EPSILON(1.0e0))

            j = 2*i + 1
            
            eps1 = ((x1**j)/(fatorial1(j))) * (-1)**i

            vp1 = vp1 + eps1
            i = i + 1

            IF (i == 100000) THEN
                PRINT *, "Algo está errado."
                stop
            END IF

        END DO

        ! Como ESPECIFICADO no enunciado, a precisão da aproximação será dada por
        ! ε/v_p, logo:

        precisao1(a) = ABS(eps1) / ABS(vp1)

    END DO

    ! Calculo da serie para 4 valores de x (0.1, 0.2, 0.3, 0.4) em precisão dupla:

    DO b = 1, 4

        ! Inicializando variáveis

        x2 = 0.1d0 * b
        eps2 = 2.0d0 * EPSILON(1.0d0)
        vp2 = 0.0d0

        ! Inicializando contadores:
        
        i = 0

        DO WHILE(ABS(eps2) > EPSILON(1.0d0))
            
            j = 2*i + 1
            
            eps2 = ((x2**j)/(fatorial2(j))) * (-1)**i

            vp2 = vp2 + eps2
            i = i + 1

            IF (i == 100000) THEN
                PRINT *, "Algo está errado."
                stop
            END IF

        END DO

        precisao2(b) = ABS(eps2) / ABS(vp2)

    END DO

    ! Impressão da tabela com os resultados:

    DO k = 1, 4
        PRINT *, 0.1e0 * k, precisao1(k), precisao2(k)
    END DO

    PRINT *, "As funções trigonométricas não possuem divergência muito grande em relação &
    &as séries, uma vez que, escolhendo-se uma precisão suficientemente pequena, já é possível &
    &obter ótimas aproximações para as funções, neste caso, a função seno. Por isso, deve-se &
    &utilizar tais aproximações quando for preciso, já que não existe uma perda consideravel de&
    &precisão e não exige muito poder de processamento do computador."

END PROGRAM exer02

! Calculo do fatorial em precisão simples:

REAL(4) FUNCTION fatorial1(n)
implicit none

    INTEGER, intent(in) :: n
    INTEGER :: i

    fatorial1 = 1.0e0

    do i = 2, n
        fatorial1 = fatorial1 * i
    end do

END FUNCTION fatorial1

! Calculo do fatorial em precisão dupla:

REAL(8) FUNCTION fatorial2(n)
implicit none

    INTEGER, intent(in) :: n
    INTEGER :: i

    fatorial2 = 1.0d0

    do i = 2, n
        fatorial2 = fatorial2 * i
    end do

END FUNCTION fatorial2





