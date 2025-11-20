#include <iostream>
#include <fstream>
#include <vector>
#include <cstdint>


#include "Packet.hpp"
#include "PacketFactory.hpp"


int main() {
  const std::string filename = "inputs/trace.bin";

  // Open file in binary mode
  std::ifstream file(filename, std::ios::binary);
  if (!file) {
    std::cerr << "Error: cannot open file " << filename << '\n';
    return 1;
  }

  PacketFactory factory = PacketFactory();
  std::vector<std::unique_ptr<Packet::Base>> packets;

  // Read all bytes into a vector
  std::vector<uint8_t> buffer((std::istreambuf_iterator<char>(file)), std::istreambuf_iterator<char>());

  // Process each byte
  for (uint8_t byte : buffer) {
    if (factory.insert(byte)) {
      packets.push_back(factory.get());
    }
  }

  // Print trace
  for (const auto& packet : packets)
    std::cout << packet->asString() << std::endl;

  return 0;
}

