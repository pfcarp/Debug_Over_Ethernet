#include <doctest/doctest.h>
#include <vector>
#include <cstdint>


#include "Deformatter.hpp"


TEST_CASE("Formatting is triggered at the 16th insertion") {
  // Create object under test
  DeformatterVector obj;
  // Byte sequence (inserts 15x Reserved packets to not induce a segfault on the factory side; i.e., 0xB?).
  std::vector<uint8_t> encoding = {0xB2, 0xB1, 0xB4, 0xB3, 0xB6, 0xB5, 0xB8, 0xB7, 0xBA, 0xB9, 0xBC, 0xBB, 0xBE, 0xBD, 0xBE};
  uint8_t aux = 0x00;

  // The first 15 byte are payload and do not trigger a deformatting (i.e., are just buffered)
  for (uint8_t byte : encoding) {
    CHECK(!obj.insert(byte));
  }
  // Last byte triggers the defomatting
  CHECK(obj.insert(aux));
}

TEST_CASE("Check ID byte detection (v1)") {
  // Create object under test
  DeformatterVector obj;
  // Auxiliary register
  uint8_t aux = 0xAA;

  for (uint8_t offset = 0; offset < 8; offset++) {
    bool res = obj.toInsertInPrevious(aux, offset);
    if (offset%2)
      CHECK(res);
    else
      CHECK(!res);
  }
}

TEST_CASE("Check ID byte detection (v2)") {
  // Create object under test
  DeformatterVector obj;
  // Auxiliary register
  uint8_t aux = 0x55;

  for (uint8_t offset = 0; offset < 8; offset++) {
    bool res = obj.toInsertInPrevious(aux, offset);
    if (offset%2)
      CHECK(!res);
    else
      CHECK(res);
  }
}

TEST_CASE("Insert 15 reserved in source 0.") {
  // Create object under test
  DeformatterVector obj;

  // Byte sequence (inserts 15x Reserved packets to not induce a segfault on the factory side; i.e., 0xB?).
  std::vector<uint8_t> encoding = {0xB2, 0xB1, 0xB4, 0xB3, 0xB6, 0xB5, 0xB8, 0xB7, 0xBA, 0xB9, 0xBC, 0xBB, 0xBE, 0xBD, 0xBE};
  uint8_t aux = 0x00;

  // The first 15 byte are payload and do not trigger a deformatting (i.e., are just buffered)
  for (uint8_t byte : encoding) {
    CHECK(!obj.insert(byte));
  }
  // Last byte triggers the defomatting
  CHECK(obj.insert(aux));

  // Check length of stream 0: must be 15, others must be 0
  CHECK(obj.factories[0].packets.size() == 15);
  CHECK(obj.factories[1].packets.size() ==  0);
  CHECK(obj.factories[2].packets.size() ==  0);
  CHECK(obj.factories[3].packets.size() ==  0);
}

TEST_CASE("Insert 8 reserved in source 0 and 6 reserved in source 1.") {
  // Create object under test
  DeformatterVector obj;

  // Byte sequence (inserts 15x Reserved packets to not induce a segfault on the factory side; i.e., 0xB?).
  //                               0   , 1   , 2   , 3   , 4   , 5   , 6   , 7   , 8   , 9   , 10  , 11  , 12  , 13  , 14
  //                               Data, Data, Data, Data, Data, Data, Data, Data, ID 1, Data, Data, Data, Data, Data, Data
  std::vector<uint8_t> encoding = {0xB2, 0xB1, 0xB4, 0xB3, 0xB6, 0xB5, 0xB8, 0xB7, 0x03, 0xB9, 0xBC, 0xBB, 0xBE, 0xBD, 0xBE};
  uint8_t aux = 0x00;

  // The first 15 byte are payload and do not trigger a deformatting (i.e., are just buffered)
  for (uint8_t byte : encoding) {
    CHECK(!obj.insert(byte));
  }
  // Last byte triggers the defomatting
  CHECK(obj.insert(aux));

  // Check length of factories
  CHECK(obj.factories[0].packets.size() ==  8);
  CHECK(obj.factories[1].packets.size() ==  6);
  CHECK(obj.factories[2].packets.size() ==  0);
  CHECK(obj.factories[3].packets.size() ==  0);
}

TEST_CASE("Insert 9 reserved in source 0 and 5 reserved in source 1 (AUX indicates that insertion must be done to previous stream(i.e., 0)).") {
  // Create object under test
  DeformatterVector obj;

  // Byte sequence (inserts 15x Reserved packets to not induce a segfault on the factory side; i.e., 0xB?).
  //                               0   , 1   , 2   , 3   , 4   , 5   , 6   , 7   , 8   , 9   , 10  , 11  , 12  , 13  , 14
  //                               Data, Data, Data, Data, Data, Data, Data, Data, ID 1, Data, Data, Data, Data, Data, Data
  std::vector<uint8_t> encoding = {0xB2, 0xB1, 0xB4, 0xB3, 0xB6, 0xB5, 0xB8, 0xB7, 0x03, 0xB9, 0xBC, 0xBB, 0xBE, 0xBD, 0xBE};
  uint8_t aux = 0x10;

  // The first 15 byte are payload and do not trigger a deformatting (i.e., are just buffered)
  for (uint8_t byte : encoding) {
    CHECK(!obj.insert(byte));
  }
  // Last byte triggers the defomatting
  CHECK(obj.insert(aux));

  // Check length of factories
  CHECK(obj.factories[0].packets.size() ==  9);
  CHECK(obj.factories[1].packets.size() ==  5);
  CHECK(obj.factories[2].packets.size() ==  0);
  CHECK(obj.factories[3].packets.size() ==  0);
}

TEST_CASE("Insert 2 reserved in source 0, 2 reserved in source 1, 3 reserved in source 2, 1 reserved in source 3 (v1).") {
  // Create object under test
  DeformatterVector obj;

  // Byte sequence (inserts 15x Reserved packets to not induce a segfault on the factory side; i.e., 0xB?).
  //                               0   , 1   , 2   , 3   , 4   , 5   , 6   , 7   , 8   , 9   , 10  , 11  , 12  , 13  , 14
  //                               ID 0, Data, ID 1, Data, ID 2, Data, ID 3, Data, ID 0, Data, ID 1, Data, ID 2, Data, Data
  std::vector<uint8_t> encoding = {0x01, 0xB1, 0x03, 0xB3, 0x05, 0xB5, 0x07, 0xB7, 0x01, 0xB9, 0x03, 0xBB, 0x05, 0xBD, 0xBE};
  uint8_t aux = 0x00;

  // The first 15 byte are payload and do not trigger a deformatting (i.e., are just buffered)
  for (uint8_t byte : encoding) {
    CHECK(!obj.insert(byte));
  }
  // Last byte triggers the defomatting
  CHECK(obj.insert(aux));

  // Check length of factories
  CHECK(obj.factories[0].packets.size() ==  2);
  CHECK(obj.factories[1].packets.size() ==  2);
  CHECK(obj.factories[2].packets.size() ==  3);
  CHECK(obj.factories[3].packets.size() ==  1);
}

TEST_CASE("Insert 2 reserved in source 0, 2 reserved in source 1, 3 reserved in source 2, 1 reserved in source 3 (v2).") {
  // Create object under test
  DeformatterVector obj;

  // Byte sequence (inserts 15x Reserved packets to not induce a segfault on the factory side; i.e., 0xB?).
  //                               0   , 1   , 2   , 3   , 4   , 5   , 6   , 7   , 8   , 9   , 10  , 11  , 12  , 13  , 14
  //                               ID 0, Data, ID 1, Data, ID 2, Data, ID 3, Data, ID 0, Data, ID 1, Data, ID 2, Data, Data
  std::vector<uint8_t> encoding = {0x01, 0xB1, 0x03, 0xB3, 0x05, 0xB5, 0x07, 0xB7, 0x01, 0xB9, 0x03, 0xBB, 0x05, 0xBD, 0xBE};
  uint8_t aux = 0xFE;

  // The first 15 byte are payload and do not trigger a deformatting (i.e., are just buffered)
  for (uint8_t byte : encoding) {
    CHECK(!obj.insert(byte));
  }
  // Last byte triggers the defomatting
  CHECK(obj.insert(aux));

  // Check length of factories
  CHECK(obj.factories[0].packets.size() ==  3);
  CHECK(obj.factories[1].packets.size() ==  2);
  CHECK(obj.factories[2].packets.size() ==  2);
  CHECK(obj.factories[3].packets.size() ==  1);
}
