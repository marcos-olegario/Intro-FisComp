PROGRAM exerA
IMPLICIT NONE

    ! Variáveis de entrada:
    REAL(8) :: r, v_0, delta_t

    ! Condições iniciais:
    REAL(8) :: x_0, y_0 = 0.0d0, vx_0 = 0.0d0, vy_0 = 0.0d0

    ! Variáveis de saída e variáveis de iteração:
    REAL(8) :: x, y
    REAL(8) :: x_i, y_i, t, periodo
    INTEGER :: break, i

    ! Constantes físicas:
    REAL(8), PARAMETER :: GM_s = 4*((4*ATAN(1.0d0))**2)  

    ! Vetores auxiliares para a construção da tabela 'tabA1_out.dat':
    REAL(8), DIMENSION(9) :: vel_inicial, razao, dist_sol
    CHARACTER(len=10), DIMENSION(9):: nomes_planetas

    ! Nome dos planetas e respectivas distâncias ao sol:
    nomes_planetas(1) = "Mercúrio"
    dist_sol(1) = 0.39d0
    nomes_planetas(2) = "Vênus"
    dist_sol(2) = 0.72d0
    nomes_planetas(3) = "Terra"
    dist_sol(3) = 1.00d0
    nomes_planetas(4) = "Marte"
    dist_sol(4) = 1.52d0
    nomes_planetas(5) = "Júpiter"
    dist_sol(5) = 5.20d0
    nomes_planetas(6) = "Saturno"
    dist_sol(6) = 9.24d0
    nomes_planetas(7) = "Urano"
    dist_sol(7) = 19.19d0
    nomes_planetas(8) = "Netuno"
    dist_sol(8) = 30.06d0
    nomes_planetas(9) = "Plutão"
    dist_sol(9) = 39.56d0

    ! Abrindo os arquivos de saída:
    OPEN(UNIT=8, FILE='trajA1_out.dat', status='NEW', ACTION='write')
    OPEN(UNIT=9, FILE='tabA1_out.dat', status='NEW', ACTION='write')

    ! Lendo a entrada do programa:
    READ(*,*) r
    READ(*,*) v_0
    READ(*,*) delta_t

    ! Aplicando as condições iniciais:
    x_0 = r
    y_0 = 0.0d0
    vx_0 = 0.0d0
    vy_0 = 0.0d0
    t = 0.0d0

    ! Primeira iteração:
    y = y_0 + (v_0 * delta_t)
    x = x_0

    ! Escrevendo primeira linha no arquivo:
    WRITE(8,*) t, x_0, y_0      ! (tempo, x_0, y_0)

    ! Iterações sucessivas:
    break = 0

    DO WHILE (break == 0)

        r = SQRT(x**2 + y**2)

        x_i = 2.0d0 * x - x_0 - ((GM_s * x)/(r**3)) * delta_t**2
        y_i = 2.0d0 * y - y_0 - ((GM_s * y)/(r**3)) * delta_t**2

        IF (y_i > 0 .AND. y < 0) THEN
            break = 1
        END IF

        x_0 = x
        y_0 = y
        x = x_i
        y = y_i

        t = t + delta_t

        WRITE(8,*) t, x, y

    END DO

    periodo = t

    ! Imprimindo resposta:
    PRINT*, 'Repetindo o programa para diferentes valores de delta_tempo'&
    ', vemos que para valores maiores que 0.01 o código não apresenta resultados'&
    ' satisfatórios e as órbitas se tornam instáveis, começando a não repetir. Isso'&
    ' está de acordo com a teoria para a discretização utilizada no problema,'&
    ' visto que o tempo de iteração em cada passo não deve ser maior que 1 por cento'&
    ' do período de órbita (1 ano para a Terra).'

    ! -----------------------------------------------------------------
    ! Construção da Tabela "tabA1_out.dat"

    ! Escrevendo primeira linha no arquivo:
    WRITE(9,*) "Planeta", "             v_0", "             T^2/R^3" 

    DO i=1, 9, 1

        vel_inicial(i) = SQRT(GM_s / dist_sol(i))

        ! Repetindo as condições iniciais:
        t = 0.0d0
        x_0 = dist_sol(i)
        y_0 = 0.0d0

        ! Repetindo a primeira iteração:
        x = x_0
        y = y_0 + vel_inicial(i) * delta_t
    
        ! Iterações sucessivas:
        break = 0

        DO WHILE (break == 0)

            r = SQRT(x**2 + y**2)
    
            x_i = 2.0d0 * x - x_0 - ((GM_s * x)/(r**3)) * delta_t**2
            y_i = 2.0d0 * y - y_0 - ((GM_s * y)/(r**3)) * delta_t**2
    
            IF (y_i > 0 .AND. y < 0) THEN
                break = 1
            END IF
    
            x_0 = x
            y_0 = y
            x = x_i
            y = y_i
    
            t = t + delta_t

        END DO

        razao(i) = ((t**2)/((dist_sol(i))**3))

    END DO

    ! Imprimindo a tabela:
    DO i=1, 9, 1

        WRITE(9,*) nomes_planetas(i), vel_inicial(i), razao(i)

    END DO

    ! -----------------------------------------------------------------
    ! Construção do gráfico das áreas

    ! Abaixo temos o código utilizado para a construção do gráfico. 
    ! Foi usado como entrada um arquivo .dat com os tempos, x e y do
    ! planeta Mercúrio.

    ! Abrindo o arquivo:
!    OPEN(UNIT=13, FILE='trajA1_out.dat', status='OLD', ACTION='read')

    ! Abrindo arquivos auxiliares para armazenar posição do planeta em cada quarto de tempo:
!    OPEN(UNIT=7, FILE='traj1.dat', status='NEW', ACTION='write')
!    OPEN(UNIT=10, FILE='traj2.dat', status='NEW', ACTION='write')
!    OPEN(UNIT=11, FILE='traj3.dat', status='NEW', ACTION='write')
!    OPEN(UNIT=12, FILE='traj4.dat', status='NEW', ACTION='write')

!    intervalo_t = periodo/4.0d0

!    DO WHILE (intervalo_t < periodo)
    
!        READ(13,*) intervalo_t, x, y

!        IF (intervalo_t >= (periodo/4.0d0) .AND. intervalo_t < periodo) THEN
!            WRITE(7,*) x, y
!        END IF

!        IF (intervalo_t >= 2*(periodo/4.0d0) .AND. intervalo_t < periodo) THEN
!            WRITE(10,*) x, y
!        END IF

!        IF (intervalo_t >= 3*(periodo/4.0d0) .AND. intervalo_t < periodo) THEN
!            WRITE(11,*) x, y
!        END IF

!        IF (intervalo_t <= (periodo/4.0d0) .AND. intervalo_t < periodo) THEN
!            WRITE(12,*) x, y
!        END IF
    
!    END DO

!   CLOSE(7)
!   CLOSE(10)
!   CLOSE(11)
!   CLOSE(12)
!   CLOSE(13)
    
    ! -----------------------------------------------------------------

    CLOSE(8)
    CLOSE(9)

END PROGRAM exerA