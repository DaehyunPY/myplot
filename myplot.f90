module myplot
    use kind_const
    implicit none
contains


! ==================================================
! PLOT
! ==================================================
! plot matrix --------------------------------------
subroutine plot_mat(FILE, MAT)
    integer  (i1), intent(in) :: FILE
    real     (dp), intent(in) :: MAT(1:, 1:)
    character(45), parameter  :: &
        form_elem =       '((10X,  I5, 5X), 10(ES15.5,  5X))', &
        form_line = '(99999((14X, "#", 5X), 10(10X, I5, 5X)))'
    integer(i4) :: N1, N2, N3, i, j, k
    N1 = size(MAT(:, 1))
    N2 = size(MAT(1, :))
    N3 = N2/10
    do i = 1, N1
        if(mod(i, 10_i4) == 1) write(FILE, form_line) (j, j = 1, N2)
        do k = 0, N3 -1
            write(FILE, form_elem, advance = 'NO') i, (MAT(i, j), j = 10*k +1, 10*k +10)
        end do
        if(mod(N2, 10_i4) /= 0) then
            write(FILE, form_elem, advance = 'NO') i, (MAT(i, j), j = 10*N3 +1, N2)
        end if
        write(FILE, form_elem)
    end do
end subroutine plot_mat
! plot vector --------------------------------------
subroutine plot_vec(FILE, VEC)
    integer  (i1), intent(in) :: FILE
    real     (dp), intent(in) :: VEC(1:)
    character(45), parameter  :: &
        form_elem =            '(20X, 99999(ES15.5,  5X))', &
        form_line = '((14X, "#", 5X), 99999(10X, I5, 5X))'
    integer(i4) :: N, i
    N = size(VEC(:))
    write(FILE, form_line) (i,      i = 1, N)
    write(FILE, form_elem) (VEC(i), i = 1, N)
end subroutine plot_vec
! plot diagonal element of matrix ------------------
subroutine plot_diag(FILE, MAT)
    integer  (i1), intent(in) :: FILE
    real     (dp), intent(in) :: MAT(1:, 1:)
    character(45), parameter  :: &
        form_elem =            '(20X, 99999(ES15.5,  5X))', &
        form_line = '((14X, "#", 5X), 99999(10X, I5, 5X))'
    integer(i4) :: N, i
    N = min(size(MAT(:, 1)), size(MAT(1, :)))
    write(FILE, form_line) (i,         i = 1, N)
    write(FILE, form_elem) (MAT(i, i), i = 1, N)
end subroutine plot_diag
end module myplot
