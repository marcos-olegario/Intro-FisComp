PROGRAM exer03
IMPLICIT NONE

    INTEGER :: M, j
    LOGICAL :: isPrimo
    OPEN (unit = 8, file = 'primos_out.dat')

    ! Realiza a leitura do número pelo terminal:
   
    DO
		READ(*,*) M
		IF (M >= 2)  EXIT
		WRITE(*,*)  'O valor precisa ser maior que 2.'
		WRITE(*,*)  'Tente novamente:'
    END DO
    
    ! Realiza uma iteração para saber quantos primos existem entre 1 e M:
   	
	WRITE(8,*) 2
			
	DO j = 3, M, 2 ! Aqui temos a primeira otimização de nosso programa: a iteração
				   ! de apenas números ímpares, visto que, além do dois, nenhum outro
				   ! número primo é par.
		
		IF (isPrimo(j) .eqv. .TRUE.) THEN
			WRITE(8,*) j
		END IF
			
	END DO
  
END PROGRAM exer03

! Função que identifica se um número é primo:

LOGICAL FUNCTION isPrimo(x)
IMPLICIT NONE

	! Recebe o número a ser testado:

    INTEGER, intent(in) :: x
    
    ! Definindo um iterador presente no escopo local:

    INTEGER :: i

    isPrimo = .FALSE.

    IF (x == 2 .or. x == 3) THEN
		
		isPrimo = .TRUE.
    
    ELSE IF (x > 3) THEN
		
		isPrimo = .TRUE.
		
		DO i = 2, x-1

			IF (MOD(x, i) == 0) THEN        
				isPrimo = .FALSE.
				EXIT
			END IF

		END DO
	   
    END IF

END FUNCTION isPrimo
