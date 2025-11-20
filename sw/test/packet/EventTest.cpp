#include <doctest/doctest.h>


#include "Packet.hpp"


TEST_CASE("Event packets are 'done' right after construction") {
  // Note: this packet is illegal. It only exists her for logic testing purposes.
  Packet::Event obj(0b01110000);

  // Right after creation
  CHECK(obj.isDone());

  // Check events
  CHECK(obj.hasEvent(0) == false);
  CHECK(obj.hasEvent(1) == false);
  CHECK(obj.hasEvent(2) == false);
  CHECK(obj.hasEvent(3) == false);
}

TEST_CASE("Event packets are 'done' right after construction") {
  // Note: this packet is illegal. It only exists her for logic testing purposes.
  Packet::Event obj(0b01110001);

  // Right after creation
  CHECK(obj.isDone());

  // Check events
  CHECK(obj.hasEvent(0) == true );
  CHECK(obj.hasEvent(1) == false);
  CHECK(obj.hasEvent(2) == false);
  CHECK(obj.hasEvent(3) == false);
}

TEST_CASE("Event packets are 'done' right after construction") {
  // Note: this packet is illegal. It only exists her for logic testing purposes.
  Packet::Event obj(0b01110010);

  // Right after creation
  CHECK(obj.isDone());

  // Check events
  CHECK(obj.hasEvent(0) == false);
  CHECK(obj.hasEvent(1) == true );
  CHECK(obj.hasEvent(2) == false);
  CHECK(obj.hasEvent(3) == false);
}

TEST_CASE("Event packets are 'done' right after construction") {
  // Note: this packet is illegal. It only exists her for logic testing purposes.
  Packet::Event obj(0b01110011);

  // Right after creation
  CHECK(obj.isDone());

  // Check events
  CHECK(obj.hasEvent(0) == true );
  CHECK(obj.hasEvent(1) == true );
  CHECK(obj.hasEvent(2) == false);
  CHECK(obj.hasEvent(3) == false);
}

TEST_CASE("Event packets are 'done' right after construction") {
  // Note: this packet is illegal. It only exists her for logic testing purposes.
  Packet::Event obj(0b01110100);

  // Right after creation
  CHECK(obj.isDone());

  // Check events
  CHECK(obj.hasEvent(0) == false);
  CHECK(obj.hasEvent(1) == false);
  CHECK(obj.hasEvent(2) == true );
  CHECK(obj.hasEvent(3) == false);
}

TEST_CASE("Event packets are 'done' right after construction") {
  // Note: this packet is illegal. It only exists her for logic testing purposes.
  Packet::Event obj(0b01110101);

  // Right after creation
  CHECK(obj.isDone());

  // Check events
  CHECK(obj.hasEvent(0) == true );
  CHECK(obj.hasEvent(1) == false);
  CHECK(obj.hasEvent(2) == true );
  CHECK(obj.hasEvent(3) == false);
}

TEST_CASE("Event packets are 'done' right after construction") {
  // Note: this packet is illegal. It only exists her for logic testing purposes.
  Packet::Event obj(0b01110110);

  // Right after creation
  CHECK(obj.isDone());

  // Check events
  CHECK(obj.hasEvent(0) == false);
  CHECK(obj.hasEvent(1) == true );
  CHECK(obj.hasEvent(2) == true );
  CHECK(obj.hasEvent(3) == false);
}

TEST_CASE("Event packets are 'done' right after construction") {
  // Note: this packet is illegal. It only exists her for logic testing purposes.
  Packet::Event obj(0b01110111);

  // Right after creation
  CHECK(obj.isDone());

  // Check events
  CHECK(obj.hasEvent(0) == true );
  CHECK(obj.hasEvent(1) == true );
  CHECK(obj.hasEvent(2) == true );
  CHECK(obj.hasEvent(3) == false);
}

TEST_CASE("Event packets are 'done' right after construction") {
  // Note: this packet is illegal. It only exists her for logic testing purposes.
  Packet::Event obj(0b01111000);

  // Right after creation
  CHECK(obj.isDone());

  // Check events
  CHECK(obj.hasEvent(0) == false);
  CHECK(obj.hasEvent(1) == false);
  CHECK(obj.hasEvent(2) == false);
  CHECK(obj.hasEvent(3) == true );
}

TEST_CASE("Event packets are 'done' right after construction") {
  // Note: this packet is illegal. It only exists her for logic testing purposes.
  Packet::Event obj(0b01111001);

  // Right after creation
  CHECK(obj.isDone());

  // Check events
  CHECK(obj.hasEvent(0) == true );
  CHECK(obj.hasEvent(1) == false);
  CHECK(obj.hasEvent(2) == false);
  CHECK(obj.hasEvent(3) == true );
}

TEST_CASE("Event packets are 'done' right after construction") {
  // Note: this packet is illegal. It only exists her for logic testing purposes.
  Packet::Event obj(0b01111010);

  // Right after creation
  CHECK(obj.isDone());

  // Check events
  CHECK(obj.hasEvent(0) == false);
  CHECK(obj.hasEvent(1) == true );
  CHECK(obj.hasEvent(2) == false);
  CHECK(obj.hasEvent(3) == true );
}

TEST_CASE("Event packets are 'done' right after construction") {
  // Note: this packet is illegal. It only exists her for logic testing purposes.
  Packet::Event obj(0b01111011);

  // Right after creation
  CHECK(obj.isDone());

  // Check events
  CHECK(obj.hasEvent(0) == true );
  CHECK(obj.hasEvent(1) == true );
  CHECK(obj.hasEvent(2) == false);
  CHECK(obj.hasEvent(3) == true );
}

TEST_CASE("Event packets are 'done' right after construction") {
  // Note: this packet is illegal. It only exists her for logic testing purposes.
  Packet::Event obj(0b01111100);

  // Right after creation
  CHECK(obj.isDone());

  // Check events
  CHECK(obj.hasEvent(0) == false);
  CHECK(obj.hasEvent(1) == false);
  CHECK(obj.hasEvent(2) == true );
  CHECK(obj.hasEvent(3) == true );
}

TEST_CASE("Event packets are 'done' right after construction") {
  // Note: this packet is illegal. It only exists her for logic testing purposes.
  Packet::Event obj(0b01111101);

  // Right after creation
  CHECK(obj.isDone());

  // Check events
  CHECK(obj.hasEvent(0) == true );
  CHECK(obj.hasEvent(1) == false);
  CHECK(obj.hasEvent(2) == true );
  CHECK(obj.hasEvent(3) == true );
}

TEST_CASE("Event packets are 'done' right after construction") {
  // Note: this packet is illegal. It only exists her for logic testing purposes.
  Packet::Event obj(0b01111110);

  // Right after creation
  CHECK(obj.isDone());

  // Check events
  CHECK(obj.hasEvent(0) == false);
  CHECK(obj.hasEvent(1) == true );
  CHECK(obj.hasEvent(2) == true );
  CHECK(obj.hasEvent(3) == true );
}

TEST_CASE("Event packets are 'done' right after construction") {
  // Note: this packet is illegal. It only exists her for logic testing purposes.
  Packet::Event obj(0b01111111);

  // Right after creation
  CHECK(obj.isDone());

  // Check events
  CHECK(obj.hasEvent(0) == true);
  CHECK(obj.hasEvent(1) == true);
  CHECK(obj.hasEvent(2) == true);
  CHECK(obj.hasEvent(3) == true);
}

