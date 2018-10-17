cmake_minimum_required(VERSION 3.12) # Older versions will almost certainly work but haven't been tested

project(quarantine LANGUAGES Fortran)

set(CMAKE_Fortran_FLAGS "-ffree-form -ffree-line-length-0 -O3 -mtune=native -finline-functions -fbacktrace")

enable_testing()

add_subdirectory(src)
add_subdirectory(input)
