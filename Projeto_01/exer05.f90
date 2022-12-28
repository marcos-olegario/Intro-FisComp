PROGRAM exer05
IMPLICIT NONE

    ! Declarando variável para a precisão e auxiliares
    REAL(8) :: eps, autovalor, autoval, autoval_aux, norma_vetor

    ! Declarando dimensão da matriz seguido de iteradores
    INTEGER :: n, i, j, k
    
    ! Declarando a matriz sem dimensão definida
    REAL(8), DIMENSION(:,:), ALLOCATABLE :: matriz

    ! Declarando vetores auxiliares
    REAL(8), DIMENSION(:), ALLOCATABLE :: vetor_auxiliar, vetor_auxiliar2, prd_vetorial_auxiliar

    ! Lê a partir do terminal valores de epsilon e dimensão da matriz
    READ(*,*) eps
    READ(*,*) n

    ! Realiza alocação da memória definindo a dimensão da matriz
    ALLOCATE(matriz(n,n))

    ! Realiza alocação da memória definindo a dimensão do vetor inicial e vetor auxiliar
    ALLOCATE(vetor_auxiliar(n))
    ALLOCATE(vetor_auxiliar2(n))
    ALLOCATE(prd_vetorial_auxiliar(n))

    ! Lê a partir do terminal os elementos da matriz
    DO i=1, n
        READ(*,*) (matriz(i,j), j=1, n) 
    END DO

    ! Definindo o vetor inicial como módulo igual a 1:
    DO i=1, n
        vetor_auxiliar(i) = (1.0d0 / (n**0.5))
    END DO

    ! Calculando um autovalor para a matriz e o vetor inicial
    autoval = autovalor(matriz, n, vetor_auxiliar)

    ! -------------------------------------------------------------
    ! Iteração para ralizar o método das potências:

    DO k=1, 1000000 ! Definindo um limite de iterações

        ! Realiza o produto vetorial da matriz pelo vetor
        DO i=1, n
            DO j=1, n
                prd_vetorial_auxiliar(i) = prd_vetorial_auxiliar(i) + matriz(i,j) * vetor_auxiliar(j)
            END DO
        END DO

        ! Fazendo um novo vetor auxiliar de modulo igual a 1:
        vetor_auxiliar2 = prd_vetorial_auxiliar / norma_vetor(prd_vetorial_auxiliar, n)
        
        ! Calculando um autovalor para a matriz e o segundo vetor auxiliar
        autoval_aux = autovalor(matriz, n, vetor_auxiliar2)

        ! Verifica se está dentro da precisão
        IF (ABS(autoval - autoval_aux) < eps) THEN
            EXIT
        END IF

        vetor_auxiliar = vetor_auxiliar2
        autoval = autoval_aux

    END DO

    PRINT *, autoval_aux

    DO i=1, n
        PRINT *, vetor_auxiliar2(i)
    END DO

END PROGRAM exer05

! ---------------------------------------------------------------------------

! Função que calcula o autovalor dada uma matriz e um vetor

REAL(8) FUNCTION autovalor(M, n, v)

    ! n: dimensão
    ! M: matriz
    ! v: vetor

    INTEGER, INTENT(in) :: n
    REAL(8), DIMENSION(n,n), INTENT(in) :: M
    REAL(8), DIMENSION(n), INTENT(in) :: v
    REAL(8), DIMENSION(n) :: Mv

    ! Declarando iteradores no escopo local
    INTEGER :: i, j

    ! Realiza o produto vetorial da matriz pelo vetor
    DO i=1, n
        Mv(i) = 0
        DO j=1, n
            Mv(i) = Mv(i) + M(i,j) * v(j)
        END DO
    END DO
    
    ! Calcula o produto escalar de v e Mv (corresponde ao autovalor)
    autovalor = dot_product(v, Mv)

END FUNCTION autovalor

! Função que calcula a norma de um vetor

REAL(8) FUNCTION norma_vetor(v, n)
IMPLICIT NONE

    INTEGER, INTENT(in) :: n
    REAL(8), DIMENSION(n), INTENT(in) :: v

    ! Declarando iteradores no escopo local
    INTEGER :: i

    ! Declarando uma variável auxiliar no escopo local
    REAL(8) :: aux

    aux = 0
    DO i=1, n
        aux = aux + (v(i)) ** 2
    END DO

    norma_vetor = SQRT(aux)

END FUNCTION norma_vetor