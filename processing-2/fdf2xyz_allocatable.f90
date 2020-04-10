      module m_fdf2xyz_allocatable

        implicit none

        interface alloc
          module procedure alloc_int
          module procedure alloc_real
          module procedure alloc_char

        endinterface

        interface dealloc
          module procedure dealloc_int
          module procedure dealloc_real
          module procedure dealloc_char

        endinterface

        contains

          subroutine alloc_int( maxim, array_alloc )
          implicit none
          integer, intent(in) :: maxim
          integer, allocatable :: array_alloc(:)
          allocate(array_alloc(maxim))
          endsubroutine alloc_int

          subroutine alloc_real( maxim, array_alloc1, &
              array_alloc2, array_alloc3)
          implicit none
          integer, intent(in) :: maxim
          real, allocatable :: array_alloc1(:)
          real, allocatable, optional :: array_alloc2(:)
          real, allocatable, optional :: array_alloc3(:)
          allocate(array_alloc1(maxim), array_alloc2(maxim), &
            array_alloc3(maxim))
          endsubroutine alloc_real

          subroutine alloc_char( maxim, array_alloc, charlen )
          implicit none
          integer, intent(in) :: maxim
          integer, intent(in) :: charlen
          character(:), allocatable :: array_alloc(:)

          allocate(character(charlen)::array_alloc(maxim))

          !allocate(array_alloc(maxim))
          endsubroutine alloc_char

          subroutine dealloc_int(array_alloc)
          implicit none
          integer, allocatable :: array_alloc(:)
          deallocate(array_alloc)
          endsubroutine dealloc_int

          subroutine dealloc_real(array_alloc1, &
              array_alloc2, array_alloc3)
          implicit none
          real, allocatable :: array_alloc1(:)
          real, allocatable, optional :: array_alloc2(:)
          real, allocatable, optional :: array_alloc3(:)
          deallocate(array_alloc1, array_alloc2, array_alloc3)
          endsubroutine dealloc_real

          subroutine dealloc_char(array_alloc, charlen)
          implicit none
          integer, intent(in) :: charlen
          character(:), allocatable :: array_alloc(:)
          deallocate(array_alloc)
          endsubroutine dealloc_char

          endmodule m_fdf2xyz_allocatable
