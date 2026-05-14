#include "Stream.hpp"


#include <iostream>
#include <memory>


StreamVector::StreamVector(): Stream() {}

void StreamVector::insert(uint8_t byte) {
  if (factory.insert(byte)) {
  }
}

size_t StreamVector::size() {
  return 0;
}

StreamVector::~StreamVector() {
}

Stream::~Stream() {}



StreamDispatcher::StreamDispatcher(Dispatcher& dispatcher): Stream(), dispatcher(dispatcher) {}

void StreamDispatcher::insert(uint8_t byte) {
/*
  if (factory.insert(byte)) {
    std::unique_ptr<Packet::Base> packet = factory.get();
    if (auto* a = dynamic_cast<Packet::Event*>(packet.get()))
      dispatcher.push(0, *a);
    else if (auto* a = dynamic_cast<Packet::LongAddress*>(packet.get()))
      dispatcher.push(0, *a);
    if (auto* a = dynamic_cast<Packet::ShortAddress*>(packet.get()))
      dispatcher.push(0, *a);
    if (auto* a = dynamic_cast<Packet::AddressWithContext*>(packet.get()))
      dispatcher.push(0, *a);
    // packet.reset();
  }
  */
}

size_t StreamDispatcher::size() {
  return 0;
}

StreamDispatcher::~StreamDispatcher() {}

