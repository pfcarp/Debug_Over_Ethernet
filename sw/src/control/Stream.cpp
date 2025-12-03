#include "Stream.hpp"


#include <iostream>
#include <memory>


void StreamVector::insert(uint8_t byte) {
  // printf("SV insert method\n");
  if (factory.insert(byte)) {
    // printf("SV insert method inside if\n");
    packets.push_back(factory.get());
  }
}

size_t StreamVector::size() {
  return packets.size();
}

StreamVector::~StreamVector() {
  std::cout << "STREAM -----------------------" << std::endl;
  for (const auto& packet : packets)
    std::cout << packet->asString() << std::endl;
}

Stream::~Stream() {
  std::cout << "STREAM -----------------------" << std::endl;
  for (const auto& packet : packets)
    std::cout << packet->asString() << std::endl;
}



StreamDispatcher::StreamDispatcher(Dispatcher& dispatcher): Stream(), dispatcher(dispatcher) {}

void StreamDispatcher::insert(uint8_t byte) {
  if (factory.insert(byte)) {
    std::unique_ptr<Packet::Base> packet = factory.get();
    dispatcher.push(0, *packet);
    packet.reset();
  }
}

size_t StreamDispatcher::size() {
  return 0;
}

StreamDispatcher::~StreamDispatcher() {}

