set(EXE_NAME projected_ranges)
add_executable(${EXE_NAME})

target_sources(${EXE_NAME}
    PRIVATE
    main.cpp
)

# Add compiler flags
if(MSVC)
    target_compile_options(${EXE_NAME} PRIVATE /W3 /WX)
else()
    # target_compile_options(${EXE_NAME} PRIVATE -Wall -Wextra -pedantic -Werror)
    target_compile_options(${EXE_NAME} PRIVATE -Wall -Wextra -pedantic -fconcepts-diagnostics-depth=100)
endif()

# Add compiler definitions
# target_compile_definitions()
