PROGRAM exerC
IMPLICIT NONE

    ! Defininco variáveis para a função e sua derivada
    REAL(8) :: f, df

    ! Definindo valores das 3 raízes para cada um dos 3 métodos:
    REAL(8):: dir1, dir2, dir3, nr1, nr2, nr3, sec1, sec2, sec3

    ! Variáveis para os chutes iniciais:
    REAL(8) :: x0_1_bd, x0_2_bd, x0_3_bd, x0_1_nr, x0_2_nr, x0_3_nr
    REAL(8) :: x0_1, x0_2, x0_3, x1_1, x1_2, x1_3
    
    ! Definindo incrmento a ser usado no método de busca direta:
    REAL(8) :: inc = 0.01d0

    ! Definindo o número de iterações e um iterador:
    INTEGER :: n,i

    ! Iniciando leitura do arquivo:
    OPEN(UNIT=9, FILE='tabC_out.dat', STATUS='NEW', ACTION='WRITE')

    ! Lendo número de valores 
    READ(*,*) n

    WRITE(9,*) "iter dir1 dir2 dir3 NR1 NR2 NR3 sec1 sec2 sec3"
    

    ! Iniciando cálculo das raízes:

    ! Chutes iniciais para as raízes para o método de Busca Direta:

    x0_1_bd = -1.2d0
    x0_2_bd = 0.4d0
    x0_3_bd = 1.75d0

    ! Chutes iniciais para as raízes para o método de Newton-Raphson:
    x0_1_nr = -1.1d0
    x0_2_nr = 0.6d0
    x0_3_nr = 1.7d0

    ! Chutes iniciais para as raízes para o método da secante:
    x0_1 = -1.1d0
    x0_2 = 0.6d0
    x0_3 = 1.7d0

    x1_1 = -1d0
    x1_2 = 0.2d0
    x1_3 = 1.6d0

    DO i = 1, n

        ! Método de Busca Direta:

        IF (f(x0_1_bd - inc) > 0) THEN
            x0_1_bd = x0_1_bd - inc
          ELSE
            x0_1_bd = x0_1_bd
        END IF
        dir1 = x0_1_bd

        IF (f(x0_2_bd + inc) > 0) THEN
            x0_2_bd = x0_2_bd + inc
          ELSE
            x0_2_bd = x0_2_bd
        END IF
        dir2 = x0_2_bd

        IF (f(x0_3_bd + inc) < 0) THEN
            x0_3_bd = x0_3_bd + inc
          ELSE
            x0_3_bd = x0_3_bd
        END IF
        dir3 = x0_3_bd


        ! Método de Newton-Raphson:

        NR1 = x0_1_nr - ((f(x0_1_nr))/(df(x0_1_nr))) ! Primeira raiz
        x0_1_nr = NR1

        NR2 = x0_2_nr - ((f(x0_2_nr))/(df(x0_2_nr))) ! Segunda raiz
        x0_2_nr = NR2

        NR3 = x0_3_nr - ((f(x0_3_nr))/(df(x0_3_nr))) ! Terceira raiz
        x0_3_nr = NR3
    

        ! Método da Secante:

        sec1 = x1_1 - f(x1_1)*((x1_1 - x0_1)/((f(x1_1)) - (f(x0_1)))) ! Primeira raiz
        x0_1 = x1_1
        x1_1 = sec1
        
        sec2 = x1_2 - f(x1_2)*((x1_2 - x0_2)/((f(x1_2)) - (f(x0_2)))) ! Segunda raiz
        x0_2 = x1_2
        x1_2 = sec2
        
        sec3 = x1_3 - f(x1_3)*((x1_3 - x0_3)/((f(x1_3)) - (f(x0_3)))) ! Terceira raiz
        x0_3 = x1_3
        x1_3 = sec3

        WRITE(9,*) i, dir1, dir2, dir3, NR1, NR2, NR3, sec1, sec2, sec3
        
    END DO

END PROGRAM exerC


! Função f(x) = x^3 − x^2 − 2x + 1
REAL(8) FUNCTION f(x)
IMPLICIT NONE

    REAL(8), INTENT(IN) :: x

    f = x**3 - x**2 - 2*x +1

END FUNCTION f


! Função para o cálculo analítico da função f no ponto x:
REAL(8) FUNCTION df(x)
IMPLICIT NONE

    REAL(8), INTENT(IN) :: x

    df = 3.0d0*x**2 - 2*x - 2

END FUNCTION df