#include <iostream>
#include <fstream>
#include <vector>
#include <cstdint>
#include <chrono>
#include <sys/mman.h>


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

  // pre-allocate all pages.
  mlockall(MCL_CURRENT | MCL_FUTURE);
  
  auto start = std::chrono::steady_clock::now();
#if 0
  for (uint8_t byte : buffer) {
    deformatter.insert(byte);
  }
#else
  uint8_t * chunk;
  for (chunk = buffer.data(); chunk < buffer.data() + buffer.size(); chunk += 16) {
      deformatter.insert_bytes(chunk, 16);
  }
#endif
  auto end = std::chrono::steady_clock::now();

  // Determine time delta
  std::chrono::duration<double> elapsed_seconds = end-start;
  double elapsed = elapsed_seconds.count();

  // Compute metrics
  double bytePerPacket = buffer.size()/static_cast<double>(deformatter.factories[0].packets.size());
  double bytePerNSec   = buffer.size()/elapsed;
  double packetPerNSec = deformatter.factories[0].packets.size()/elapsed;

  // Print as CSV
  std::cout << "bytes,seconds,packets,bytePerPacket,bytePerSec,packetPerSec" << std::endl;
  std::cout << buffer.size() << "," << elapsed << "," << deformatter.factories[0].packets.size() << "," << bytePerPacket << "," << bytePerNSec << "," << packetPerNSec << std::endl;

  return 0;
}

