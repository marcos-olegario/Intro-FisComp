PROGRAM exerA
IMPLICIT NONE

    ! Definindo variáveis da função e derivadas de 1ª, 2ª e 3ª ordem:
    REAL(8) :: f, deriv1, deriv2, deriv3
    REAL(8) :: deriv1_sim_3, deriv1_frent_2, deriv1_tras_2
    REAL(8) :: deriv2_sim_3, deriv2_sim_5, deriv3_5

    ! Definindo valor de x_0:
    REAL(8) :: x0 = (1.0d0/3.0d0)

    ! Definindo inteiros:
    INTEGER :: nh, i

    ! Definindo valores de h
    REAL(8), DIMENSION(:), ALLOCATABLE :: h

    ! Iniciando leitura dos arquivos:
    OPEN(UNIT=8, FILE='tabA_in.dat', STATUS='OLD', ACTION='READ')
    OPEN(UNIT=9, FILE='tabA_out.dat', STATUS='NEW', ACTION='WRITE')

    ! Lendo número de valores 
    READ(8,*) nh

    ! Realizando alocação da memória definindo a dimensão do array h:
    ALLOCATE(h(nh))

    ! Lendo valores de h:
    READ(8,*) (h(i), i=1, nh)

    ! Escrevendo as derivadas:
    deriv1 = (EXP(4*x0) * (8*COS(x0/2) - SIN(x0/2)))/2
    deriv2 = (EXP(4*x0) * (63*COS(x0/2) - 16*SIN(x0/2)))/4
    deriv3 = (EXP(4*x0) * (488*COS(x0/2) - 191*SIN(x0/2)))/8
    
    WRITE(9,*) "h deriv1_sim_3_pts deriv1_frent_2_pts deriv1_tras_2_pts deriv2_sim_3_pts deriv2_sim_5_pts deriv3_antisim_5_pts"
    DO i=1, nh

        deriv1_sim_3 = deriv1 - ((f(1, h(i)) - f(-1,h(i)))/(2*h(i)))
        deriv1_frent_2 = deriv1 - ((f(1,h(i)) - f(0,h(i)))/h(i))
        deriv1_tras_2 = deriv1 - ((f(0,h(i)) - f(-1,h(i)))/h(i))

        deriv2_sim_3 = deriv2 - ((f(1,h(i)) - 2*f(0,h(i)) + f(-1,h(i)))/(h(i))**2)
        deriv2_sim_5 = deriv2 - ((-f(2,h(i)) + 16*f(1,h(i)) - 30*f(0,h(i)) + 16*f(-1,h(i)) - f(-2,h(i)))/(12*(h(i))**2))

        deriv3_5 = deriv3 - ((f(2,h(i)) - 2*f(1,h(i)) + 2*f(-1,h(i)) - f(-2,h(i)))/(2*(h(i))**3))

        WRITE(9,*) h(i), deriv1_sim_3, deriv1_frent_2, deriv1_tras_2, deriv2_sim_3, deriv2_sim_5, deriv3_5

    END DO

    ! Respostas e justificativas:

    PRINT *, "  Os melhores valores de h para cada caso são descritos abaixo, a justificativa adotada " //&
    "para escolhê-los foi determinar o h correspondente ao menor desvio, ou seja, a menor " //&
    "diferença absoluta entre a derivada e a aproximação escolhida em cada caso."

    PRINT *, "Melhor valor para a derivada simétrica de 3 pontos: ", '0.000001'
    PRINT *, "Melhor valor para a derivada para frente de 2 pontos: ", '0.00000001'
    PRINT *, "Melhor valor para a derivada para trás de 2 pontos: ", '0.00000001'
    PRINT *, "Melhor valor para a derivada segunda simétrica de 3 pontos: ", '0.00005'
    PRINT *, "Melhor valor para a derivada segunda simétrica de 5 pontos: ", '0.0005'
    PRINT *, "Melhor valor para a derivada terceira anti-simétrica de 5 pontos: ", '0.0005'

END PROGRAM exerA

! Função f(x) = exp(4x) cos(x/2) para x = 1/3:

REAL(8) FUNCTION f(n, h)
IMPLICIT NONE

    INTEGER, INTENT(IN) :: n
    REAL(8), INTENT(IN) :: h
    REAL(8):: x0

    x0 = (1.0d0/3.0d0)

    f = EXP(4*(x0+n*h)) * COS((x0+n*h)/2)

END FUNCTION f
