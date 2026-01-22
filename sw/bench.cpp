#include <iostream>
#include <fstream>
#include <vector>
#include <cstdint>
#include <chrono>


#include "Deformatter.hpp"


int main(int argc, char* argv[]) {

  // Check parameter is present
  if (argc != 2) {
    std::cerr << "Usage: " << argv[0] << " <filename>" << std::endl;
    return 1;
  }
  const std::string filename = argv[1];

  // Open file in binary mode
  std::ifstream file(filename, std::ios::binary);
  if (!file) {
    std::cerr << "Error: cannot open file " << filename << '\n';
    return 1;
  }


  // Read all bytes into a vector
  std::vector<uint8_t> buffer((std::istreambuf_iterator<char>(file)), std::istreambuf_iterator<char>());
  // Create deformatter
  DeformatterVector deformatter;

  auto start = std::chrono::steady_clock::now();
  for (uint8_t byte : buffer) {
    deformatter.insert(byte);
  }
  auto end = std::chrono::steady_clock::now();

  // Determine time delta
  std::chrono::duration<double> elapsed_seconds = end-start;
  double elapsed = elapsed_seconds.count();

  // Compute metrics
  double bytePerPacket = buffer.size()/static_cast<double>(deformatter.streams[0]->size());
  double bytePerNSec   = buffer.size()/elapsed;
  double packetPerNSec = deformatter.streams[0]->size()/elapsed;

  // Print as CSV
  std::cout << "bytes,seconds,packets,bytePerPacket,bytePerSec,packetPerSec" << std::endl;
  std::cout << buffer.size() << "," << elapsed << "," << deformatter.streams[0]->size() << "," << bytePerPacket << "," << bytePerNSec << "," << packetPerNSec << std::endl;

  return 0;
}

