#ifndef TOOLS_HPP
#define TOOLS_HPP

#include <chrono>
#include <iostream>

#define MEASURE_TIME(block)                                                   \
do {                                                                          \
    auto start = std::chrono::high_resolution_clock::now();                   \
    { block }                                                                 \
    auto end = std::chrono::high_resolution_clock::now();                     \
    auto duration = std::chrono::duration_cast<std::chrono::nanoseconds>(     \
        end - start).count();                                                  \
    std::cout << "Time: " << duration << " ns" << std::endl;                  \
} while (0);

#endif // TOOLS_HPP
#define TOOLS_HPP
