#pragma once


#include <cstdint>
#include <mutex>
#include <span>
#include <string>


#include <sqlite3.h>


// Single sqlite database shared by every PacketFactory.
//   PacketTypes(header, name, quantity): one row per header byte, quantity is
//                                         kept up to date by a trigger on Packets.
//   Packets(header, data, timestamp, stream): one row per decoded packet.
// Hex values are stored as text ("0x..."); timestamps are zero padded so they sort.
class PacketDatabase {

  private:
    // Attributes
    static constexpr const char* path = "packets.db";
    static constexpr uint32_t batchSize = 4096;
    sqlite3* db = nullptr;
    sqlite3_stmt* insertType = nullptr;
    sqlite3_stmt* insertPacket = nullptr;
    uint32_t pending = 0;
    std::mutex lock;
    // Methods
    PacketDatabase();
    ~PacketDatabase();
    bool exec(const char* sql);
    void commit();

  public:
    // Methods
    static PacketDatabase& instance();
    PacketDatabase(const PacketDatabase&) = delete;
    PacketDatabase& operator=(const PacketDatabase&) = delete;
    void addType(uint8_t header, const std::string& name);
    void addPacket(uint8_t header, std::span<const uint8_t> data, uint64_t timestamp, uint32_t stream);
    void flush();

};
