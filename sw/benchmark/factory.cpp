#include <iostream>
#include <cstdint>
#include <chrono>


#include "PacketFactory.hpp"


int main(int argc, char* argv[]) {

  constexpr uint64_t iterations = 32*1024*1024;
  PacketFactory factory = PacketFactory();

  std::cout << "ID,seconds" << std::endl;

  for (uint64_t byte = 0; byte < 256; byte++) {
    uint8_t id = static_cast<uint8_t>(byte);

    auto start = std::chrono::steady_clock::now();
    for (uint64_t iteration = 0; iteration < iterations; iteration++) {
      factory.insert(id);
    }
    auto end = std::chrono::steady_clock::now();

    // Determine time delta
    std::chrono::duration<double> elapsed_seconds = end-start;
    double elapsed = elapsed_seconds.count();

    std::cout << static_cast<uint32_t>(id) << "," << elapsed << "," << iterations << std::endl;
  }

  return 0;
}

