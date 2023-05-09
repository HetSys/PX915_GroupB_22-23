!>@brief this is a module
!! this is an explanation of what the module contains
Module mymodule

    implicit none
    save

    !> @var integer module_int
    !!module integer variable
    Integer :: module_int

    contains

    !> @brief Generate hello world subroutine.
    !! @details Subroutine that:
    !! 1. prints "Hello World"
    !! 2. prints "Module_int is", <module_int>
    Subroutine hello_world_sub()
        Print *, "Hello World"
        Print *, "Module_int is", module_int
    End Subroutine hello_world_sub

End Module mymodule

Program doxy_fort_test

    Use mymodule

    implicit none

    module_int = 1234
    Call hello_world_sub()
       
End Program doxy_fort_test
