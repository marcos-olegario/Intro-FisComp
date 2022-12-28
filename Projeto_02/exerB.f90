PROGRAM exerB
IMPLICIT NONE

    ! Definindo variáveis da função, integral e limites de integração:
    REAL(8) :: f, integral
    REAL(8) :: a = 0.0d0
    REAL(8) :: b = 1.0d0

    ! Variáveis para os valores das integrais:
    REAL(8) :: int_trap, int_simp, int_bode

    ! Varáveis para os desvios de cada método:
    REAL(8) :: desv_trap, desv_simp, desv_bode

    ! Varáveis para os pontos do intervalo:
    REAL(8) :: x_back, x_0, x_front
    REAL(8) :: x0, x1, x2, x3, x4

    ! Inteiros auxiliares e contadores:
    INTEGER :: n_, i, j, k

    ! Variável h:
    REAL(8) :: h

    ! Vetor com valores de N
    INTEGER, DIMENSION(:), ALLOCATABLE :: N

    ! Iniciando leitura dos arquivos:
    OPEN(UNIT=8, FILE='tabB_in.dat', STATUS='OLD', ACTION='READ')
    OPEN(UNIT=9, FILE='tabB_out.dat', STATUS='NEW', ACTION='WRITE')

    ! Lendo número de valores 
    READ(8,*) n_

    ! Realizando alocação da memória definindo a dimensão do array N:
    ALLOCATE(N(n_))

    ! Lendo valores de N:
    READ(8,*) (N(i), i=1, n_)

    ! Resolvendo a integral de forma analítica obtemos:
    integral = COS(1.0d0/2.0d0) - (1.0d0/3.0d0)*COS(3.0d0/2.0d0) - 2.0d0/3.0d0

    WRITE(9,*) "N       ", "h       ", "desv_trap       ", "desv_simp       ", "desv_bode"
    DO i=1, n_

        h = (b-a)/N(i)

        ! Valores das integrais antes da iteração:

        int_trap = 0.0d0
        int_simp = 0.0d0
        int_bode = 0.0d0

        ! Cálculo da integral via regra do trapézio e de Simpson (desconsiderando o desvio O(h⁷)):

        DO j=0, N(i)-1, 2 ! 2 pois os intervalos possuem extensão 2h.

            ! Abscissas necessárias para o cálculo das integrais nos dois métodos:
            x_back = a + j*h + 0*h ! a expressão 0*h está aqui somente para organizar meu raciocínio
            x_0 = a + j*h + h
            x_front = a + j*h + 2.0d0*h

            int_trap = int_trap + ((h/2.0d0) * (f(x_front) + 2.0d0*f(x_0) + f(x_back)))

            int_simp = int_simp + ((h/3.0d0) * (f(x_front) + 4.0d0*f(x_0) + f(x_back)))

        END DO

        ! Cálculo da integral via regra de Bode (desconsiderando o desvio O(h⁷)):

        DO k = 0, N(i)-1, 4

            ! Abscissas necessárias para o cálculo da integral:
            x0 = a + k*h
            x1 = x0 + h
            x2 = x0 + 2.0d0*h
            x3 = x0 + 3.0d0*h
            x4 = x0 + 4.0d0*h

            int_bode = int_bode + ((2.0d0*h/45.0d0) * (7.0d0*f(x0) + 32.0d0*f(x1) + 12.0d0*f(x2) + 32.0d0*f(x3) + 7.0d0*f(x4)))

        END DO

        ! Cálculo dos desvios:
        desv_trap = integral - int_trap
        desv_simp = integral - int_simp
        desv_bode = integral - int_bode

        ! Comandos auxiliares apenas para conferir as respostas e o andamento do código:
        !WRITE(9,*) "integral: ", integral
        !WRITE(9,*) "Trapezio: ", int_trap
        !WRITE(9,*) "Simpson: ", int_simp
        !WRITE(9,*) "Bode: ", int_bode
        !WRITE(9,*) "Desvios:"
        WRITE(9,*) N(i), h, desv_trap, desv_simp, desv_bode ! Resposta final
        !WRITE(9,*) " "

    END DO

    ! Resposta terminal:
    PRINT *, "Para todos os casos, o melhor valor de N é dado como aquele onde a integral calculada " //&
    "através do método apresenta o menor desvio em relação ao valor da integral calculado analíticamente. " //&
    "Para todos os casos, o melhor N foi de de 4096."

END PROGRAM exerB

! Função f(x) = sin(x/2) cos(x)

REAL(8) FUNCTION f(x)
IMPLICIT NONE

    REAL(8), INTENT(IN) :: x

    f = SIN(x/2) * COS(x)

END FUNCTION f