PROGRAM exerB
IMPLICIT NONE

    ! Variáveis de entrada:
    REAL(8) :: r_t, r_tj, r_j, t, delta_t, fator

    ! Condições Terra:
    REAL(8) :: v_0t, x_0t, y_0t, x_t, y_t, x_ti, y_ti 

    ! Condições Júpiter:
    REAL(8) :: v_0j, x_0j, y_0j, x_j, y_j, x_ji, y_ji 

    ! Variável de iteração:
    INTEGER :: i

    ! Constantes físicas:
    REAL(8), PARAMETER :: GM_s = 4*((4*ATAN(1.0d0))**2)  
    REAL(8), PARAMETER :: GM_j = (GM_s * (1.9d0*10e27))/(2.0d0*10e30)
    REAL(8), PARAMETER :: GM_t = (GM_s * (6.0d0*10e24))/(2.0d0*10e30)

    ! Abrindo o arquivo de saída:
    OPEN(UNIT=8, FILE='trajB1_out.dat', status='NEW', ACTION='write')
   
    ! Aplicando as condições iniciais:
    t = 0.0d0
    delta_t = 0.0001d0

    r_t = 1.0d0
    r_j = 5.2d0
    v_0t = 2*(4*ATAN(1.0d0)) ! v_0 = 2pi
    v_0j = SQRT(GM_s/r_j) ! v_0 para Júpiter

    x_0t = r_t
    y_0t = 0.0d0
    x_0j = r_j
    y_0j = 0.0d0

    ! Primeira iteração:
    x_t = x_0t
    y_t = y_0t + (v_0t * delta_t)
    x_j = x_0j
    y_j = y_0j + (v_0j * delta_t)

    ! Recebendo o fator multiplicativo da massa de Júpiter:
    PRINT*, "Fator Multiplicativo: "
    READ(*,*) fator

    ! Escrevendo primeira linha no arquivo:
    WRITE(8,*) t, x_0t, y_0t, x_0j, y_0j

    ! Iniciando loop do movimento orbital:
    DO i=1, 200000

        r_t = SQRT(x_t**2 + y_t**2)
        r_j = SQRT(x_j**2 + y_j**2)
        r_tj = SQRT((x_t - x_j)**2 + (y_t - y_j)**2)

        x_ti = 2.0d0*x_t - x_0t - ((GM_s * x_t)/(r_t**3) + (fator * GM_j * (x_t - x_j))/(r_tj**3)) * delta_t**2
        y_ti = 2.0d0*y_t - y_0t - ((GM_s * y_t)/(r_t**3) + (fator * GM_j * (y_t - y_j))/(r_tj**3)) * delta_t**2

        x_ji = 2.0d0*x_j - x_0j - ((GM_s * x_j)/(r_j**3) + (GM_t * (x_j - x_t))/(r_tj**3)) * delta_t**2
        y_ji = 2.0d0*y_j - y_0j - ((GM_s * y_j)/(r_j**3) + (GM_t * (y_j - y_t))/(r_tj**3)) * delta_t**2

        x_0t = x_t
        y_0t = y_t
        x_t = x_ti
        y_t = y_ti

        x_0j = x_j
        y_0j = y_j
        x_j = x_ji
        y_j = y_ji

        t = t + delta_t

        WRITE(8,*) t, x_t, y_t, x_j, y_j

    END DO

    CLOSE(8)

END PROGRAM exerB