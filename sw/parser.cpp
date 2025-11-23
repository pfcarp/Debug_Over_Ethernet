#include <iostream>
#include <fstream>
#include <vector>
#include <cstdint>


#include "Deformatter.hpp"


int main() {
  const std::string filename = "inputs/trace.bin";

  // Open file in binary mode
  std::ifstream file(filename, std::ios::binary);
  if (!file) {
    std::cerr << "Error: cannot open file " << filename << '\n';
    return 1;
  }


  // Read all bytes into a vector
  std::vector<uint8_t> buffer((std::istreambuf_iterator<char>(file)), std::istreambuf_iterator<char>());

  // Process each byte
  Deformatter deformatter;
  for (uint8_t byte : buffer) {
    deformatter.insert(byte);
  }

  // Print trace
  for (const auto& stream : deformatter.streams) {
    std::cout << "STREAM -----------------------" << std::endl;
    for (const auto& packet : stream.packets)
      std::cout << packet->asString() << std::endl;
  }

  return 0;
}

