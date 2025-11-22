#include <iostream>
#include <fstream>
#include <vector>
#include <cstdint>
#include <format>


#include "Packet.hpp"
#include "PacketFactory.hpp"


class Stream {

  public:
    PacketFactory factory = PacketFactory();
    std::vector<std::unique_ptr<Packet::Base>> packets;

    void insert(uint8_t byte);

};

void Stream::insert(uint8_t byte) {
  if (factory.insert(byte)) {
    packets.push_back(factory.get());
  }
}

class Deformatter {

  private:
    // Attributes
    uint8_t counter = 0;
    std::vector<uint8_t> frame = std::vector<uint8_t>(16);
    
  public:
    // Attributes
    bool insertInPrevious = false;
    uint8_t current = 0;
    uint8_t previous = 0;
    std::vector<Stream> streams = std::vector<Stream>(4);
    // Methods
    void insert(uint8_t byte);
    void format();
    void clean();

};

void Deformatter::insert(uint8_t byte) {
  // Insert new byte
  frame[counter] = byte;
  // If frame size reached
  if (counter == 15) {
    // Format frame
    std::cout << "Formating" << std::endl;
    format();
    clean();
  }
  else {
    counter++;
  }
}

/**
 * Followed format presented in DDI0314H page 220 (Sec. 8.12.1)
 */
void Deformatter::format() {
  for (uint8_t i = 0; i < 15; i++) { // 15 becasue last byte contains carried over bits
    std::cout << std::format("0x{:02X}", frame[i]) << std::endl;
    // Inspect if odd indexed byte and check if it is an ID
    if (i%2 == 0) {
      if (frame[i]%2) {
        previous = current;
        current = frame[i] >> 1;
        insertInPrevious = (frame[15] >> i)%2;
        std::cout << "(New ID! " << static_cast<int>(current) << " @" << static_cast<int>(i) << ")" << std::endl;
        // TODO: check AUX for current or previous
      }
      else {
        streams[current].insert((frame[i] & 0xfe) | ((frame[15] >> (i/2)) & 0x01));
      }
    }
    else {
      if (insertInPrevious)
        streams[previous].insert(frame[i]);
      else
        streams[current].insert(frame[i]);
    }
  }
  std::cout << std::format("0x{:02X}", frame[15]) << std::endl;
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

