set(EXE_NAME projected_ranges)
add_executable(${EXE_NAME})

target_sources(${EXE_NAME}
    PRIVATE
    main.cpp
)

# Add compiler flags
if(CMAKE_CXX_COMPILER_ID STREQUAL "MSVC")
    target_compile_options(${EXE_NAME} PRIVATE /W3 /WX)
elseif(CMAKE_CXX_COMPILER_ID STREQUAL "Clang")
    target_compile_options(${EXE_NAME} PRIVATE -Wall -Wextra -Wpedantic -Werror)
elseif(CMAKE_CXX_COMPILER_ID STREQUAL "GNU")
    target_compile_options(${EXE_NAME} PRIVATE
        -Wall -Wextra -Wpedantic -fconcepts-diagnostics-depth=100)
endif()

# Add compiler definitions
# target_compile_definitions()
