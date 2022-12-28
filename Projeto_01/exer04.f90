PROGRAM exer04
IMPLICIT NONE

    ! Definindo array com coordenadas de cada vértice:
    REAL(4), DIMENSION(4) :: x, y, z

    ! Definindo os vetores:
    REAL(4), DIMENSION(3) :: v1, v2, v3, v4, v5

    ! Variáveis auxiliares:
    REAL(4), DIMENSION(3) :: produto_vetorial
    REAL(4) :: produto_escalar, aux, calculo_area

    ! Variáveis finais:
    REAL(4) :: volume
    REAL(4), DIMENSION(4) :: areas, areas_diferentes

    ! Definindo um iterador:
    INTEGER :: i, j

    ! Iniciando leitura dos arquivos:
    OPEN(UNIT=8, FILE="vet_in.dat", STATUS="OLD", ACTION="READ") ! Arquivo de entrada
    OPEN(UNIT=9, FILE="tetra_out.dat", STATUS="NEW", ACTION="WRITE") ! Arquivo de saída

    ! Lendo coordenadas dos vetores:
    DO i = 1, 4
        READ(8, *) x(i), y(i), z(i)
    END DO

    ! Cálculo do volume do tetraedro:
    ! O volume de um tetraedro é dado por 1/6 do volume do paralelpipedo gerado pelos mesmos vetores,
    ! ou seja, vale |(A . (B x C))| / 6.

    ! Definindo os vetores:

    ! Vetor 1 (índice 1 representa x, 2 representa y, 3 representa z):
    v1(1) = x(2) - x(1) ! x1
    v1(2) = y(2) - y(1) ! y1
    v1(3) = z(2) - z(1) ! z1

    ! Vetor 2
    v2(1) = x(3) - x(1) ! x2
    v2(2) = y(3) - y(1) ! y2
    v2(3) = z(3) - z(1) ! z2

    ! Vetor 3
    v3(1) = x(4) - x(1) ! x3
    v3(2) = y(4) - y(1) ! y3
    v3(3) = z(4) - z(1) ! z3

    ! Calculando produto vetorial de v2 por v3:
    produto_vetorial(1) = v2(2) * v3(3) - v2(3) * v3(2)
    produto_vetorial(2) = v2(3) * v3(1) - v3(3) * v2(1)
    produto_vetorial(3) = v2(1) * v3(2) - v2(2) * v3(1)

    ! Calculando produto escalar de 'produto_vetorial' por v1
    produto_escalar = produto_vetorial(1) * v1(1) + produto_vetorial(2) * v1(2) + produto_vetorial(3) * v1(3)

    ! Calculando o volume do tetraedro:
    volume = produto_escalar / 6.0e0

    WRITE(9, *) volume ! imprime o volume

    ! Cálculo da área de cada face:
    ! A área de cada face é dada pelo produto escalar de dois vetores que formam duas arestas do triângulo sobre 2

    ! Definindo dois vetores que formam as arestas restantes do tetraedro:

    v4(1) = x(3) - x(2) ! x4
    v4(2) = y(3) - y(2) ! y4
    v4(3) = z(3) - z(2) ! z4

    v5(1) = x(4) - x(2) ! x5
    v5(2) = y(4) - y(2) ! y5
    v5(3) = z(4) - z(2) ! z5

    ! Calculando as áreas de cada face:

    areas(1) = calculo_area(v1, v2)
    areas(2) = calculo_area(v1, v3)
    areas(3) = calculo_area(v2, v3)
    areas(4) = calculo_area(v4, v5)

    WRITE(9, *) (areas(1) + areas(2) +  areas(3) +  areas(4))! escreve a soma das áreas

    ! Ordenando de forma crescente o array 'areas':

    DO i = 1, 3
        DO j = i+1, 4
            IF (areas(i) >= areas(j)) THEN
                aux = areas(i)
                areas(i) = areas(j)
                areas(j) = aux
            END IF
        END DO
    END DO

    ! Verificando quantos valores diferentes de área existem:

    aux = 0.0e0
    j = 1

    DO i = 1, 4
        IF (areas(i) /= aux) THEN
            areas_diferentes(j) = areas(i)
            j = j+1
        END IF
        aux = areas(i)
    END DO

    ! Imprimindo as áreas distintas --- (j-1) se refere ao número de áreas distintas
    ! Como ESPECIFICADO NO ENUNCIADO, imprime-se as áreas em uma mesma linha.

    IF ((j-1) == 1) THEN
        WRITE(9, *) areas_diferentes(1)
    ELSEIF ((j-1) == 2) THEN
        WRITE(9, *) areas_diferentes(1), areas_diferentes(2)
    ELSEIF ((j-1) == 3) THEN
        WRITE(9, *) areas_diferentes(1), areas_diferentes(2), areas_diferentes(3)
    ELSEIF ((j-1) == 4) THEN
        WRITE(9, *) areas_diferentes
    END IF

    CLOSE(8)
    CLOSE(9)

END PROGRAM exer04

! Função que retorna o valor da área de um triângulo definido por dois vetores

REAL(4) FUNCTION calculo_area(v1, v2)
    
    REAL(4), DIMENSION(3), intent(in) :: v1, v2
    REAL(4), DIMENSION(3) :: prd_vet
    
    prd_vet(1) = v1(2) * v2(3) - v1(3) * v2(2)
    prd_vet(2) = v1(3) * v2(1) - v1(1) * v2(3)
    prd_vet(3) = v1(1) * v2(2) - v1(2) * v2(1)
    
    calculo_area = (SQRT((prd_vet(1))**2 + (prd_vet(2))**2 + (prd_vet(3))**2))/2.0e0

END FUNCTION calculo_area