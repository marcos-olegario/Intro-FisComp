program exer01
implicit none

! Definindo as variáveis auxiliares e contadores de bits:
    REAL(4) :: a1
    REAL(8) :: a2
    REAL(16) :: a3
    INTEGER :: c1, c2, c3

! Atribuindo valores 
    a1 = 1.00e0
    a2 = 1.00d0
    a3 = 1.00_16
    c1 = 0
    c2 = 0
    c3 = 0

! Iniciando o loop com a precisão simples:
    print *, "PRECISAO SIMPLES"
    do while (1.00e0 /= (1.00e0 + a1))
        a1 = a1/(2.00e0)
        c1 = c1 + 1
        print *, a1, (1.00e0 + a1)
    end do

! Iniciando o loop com a precisão dupla:
    print *, "PRECISAO DUPLA"
    do while (1.00d0 /= (1.00d0 + a2))
        a2 = a2/(2.00d0)
        c2 = c2 + 1
        print *, a2, (1.00d0 + a2)
    end do

! Iniciando o loop com a precisão quádrupla:
    print *, "PRECISAO QUADRUPLA"
    do while (1.00_16 /= (1.00_16 + a3))
        a3 = a3/(2.00_16)
        c3 = c3 + 1
        print *, a3, (1.00_16 + a3)
    end do

! Imprimindo os resultados:

    print *, c1, a1 * 2.00e0
    print *, c2, a2 * 2.00d0
    print *, c3, a3 * 2.00_16

end program exer01
