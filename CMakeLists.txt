cmake_minimum_required(VERSION 3.12) # Older versions will almost certainly work but haven't been tested

project(quarantine LANGUAGES Fortran)

set(CMAKE_Fortran_FLAGS "-O3")

enable_testing()

add_subdirectory(src)
