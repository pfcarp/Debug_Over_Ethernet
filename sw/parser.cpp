#include <iostream>
#include <fstream>
#include <vector>
#include <cstdint>
#include <format>


#include "Packet.hpp"
#include "PacketFactory.hpp"


class Deformatter {

  private:
    // Attributes
    uint8_t counter = 0;
    
  public:
    // Attributes
    std::vector<uint8_t> frame = std::vector<uint8_t>(16);    
    // Methods
    bool insert(uint8_t byte);
    void format();
    void clean();

};

bool Deformatter::insert(uint8_t byte) {
  // Insert new byte
  frame[counter] = byte;
  // If frame size reached
  if (counter == 15) {
    // Format frame
    //std::cout << "Before: ";
    //for (uint8_t b : frame)
    //  std::cout << std::format("0x{:02X}", b) << " ";
    //std::cout << std::endl;
    format();
    //std::cout << "After: ";
    //for (uint8_t b : frame)
    //  std::cout << std::format("0x{:02X}", b) << " ";
    //std::cout << std::endl;
    return true;
  }
  counter++;
  return false;
}

/**
 * Followed format presented in DDI0314H page 220 (Sec. 8.12.1)
 */
void Deformatter::format() {
  std::vector<uint8_t> newFrame;
  for (uint8_t i = 0; i < 15; i++) { // 15 becasue last byte contains carried over bits
    // Inspect if odd indexed byte and check if it is an ID
    if (i%2 == 0) {
      if (frame[i]%2) {
        //std::cout << "ID: " << static_cast<int>(frame[i] >> 1) << std::format(" (0x{:02X})", frame[i]) << std::endl;
      }
      else {
        newFrame.push_back((frame[i] & 0xfe) | ((frame[15] >> (i/2)) & 0x01));
      }
    }
    else {
      newFrame.push_back(frame[i]);
    }
  }
  frame = newFrame;
}

void Deformatter::clean() {
  counter = 0;
  frame.resize(16);
  frame.assign(frame.size(), 0);
}


int main() {
  const std::string filename = "inputs/trace.bin";

  // Open file in binary mode
  std::ifstream file(filename, std::ios::binary);
  if (!file) {
    std::cerr << "Error: cannot open file " << filename << '\n';
    return 1;
  }

  PacketFactory factory = PacketFactory();
  Deformatter deformatter = Deformatter();
  std::vector<std::unique_ptr<Packet::Base>> packets;

  // Read all bytes into a vector
  std::vector<uint8_t> buffer((std::istreambuf_iterator<char>(file)), std::istreambuf_iterator<char>());

  // Process each byte
  for (uint8_t byte : buffer) {
    if (deformatter.insert(byte)) {
      for (uint8_t b : deformatter.frame) {
        std::cout << std::format(" 0x{:02X}", b);
        if (factory.insert(b)) {
          packets.push_back(factory.get());
        }
      }
      deformatter.clean();
    }
  }
  std::cout << std::endl;

  // Print trace
  for (const auto& packet : packets)
    std::cout << packet->asString() << std::endl;

  return 0;
}

