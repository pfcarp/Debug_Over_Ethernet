#include "PacketDatabase.hpp"


#include <format>
#include <iostream>


PacketDatabase::PacketDatabase() {
  if (sqlite3_open(path, &db) != SQLITE_OK) {
    std::cerr << "PacketDatabase: cannot open " << path << ": " << sqlite3_errmsg(db) << std::endl;
    sqlite3_close(db);
    db = nullptr;
    return;
  }
  // Every run starts from a clean database // drops the tables if they exist and then remakes them. the last part might be problematic as it will trigger at every insertion rather than at the end
  //WAL: Writes are appended to a separate WAL file first. Readers and writers do not block each other, allowing concurrent reads while a write happens. Unlike other modes, WAL is persistent across database closes and reopens.
  //When synchronous is OFF (0), QDB continues without pausing as soon as it has handed data off to the operating system. If the application running QDB crashes, the data will be safe but the database might become corrupted if the operating system crashes or the computer loses power before that data has been written to the disk. However, some operations are as much as 50 or more times faster with the OFF setting.
  //sqlite does not seem to have a hexadecimal primative. Should be easier for human debugging if kept in text
  bool ok = exec(
    "PRAGMA journal_mode = WAL;"
    "PRAGMA synchronous = OFF;"
    "DROP TRIGGER IF EXISTS CountPacket;"
    "DROP TABLE IF EXISTS Packets;"
    "DROP TABLE IF EXISTS PacketTypes;"
    "CREATE TABLE PacketTypes ("
    "  header   TEXT    PRIMARY KEY,"
    "  name     TEXT    NOT NULL,"
    "  quantity INTEGER NOT NULL DEFAULT 0"
    ");"
    "CREATE TABLE Packets ("
    "  header    TEXT    NOT NULL REFERENCES PacketTypes(header),"
    "  data      TEXT    NOT NULL,"
    "  timestamp TEXT    NOT NULL,"
    "  stream    INTEGER NOT NULL"
    ");"
    "CREATE TRIGGER CountPacket AFTER INSERT ON Packets BEGIN"
    "  UPDATE PacketTypes SET quantity = quantity + 1 WHERE header = NEW.header;"
    "END;"
  );
  ok = ok && sqlite3_prepare_v2(db,
    "INSERT OR REPLACE INTO PacketTypes (header, name, quantity) VALUES (?1, ?2, 0);",
    -1, &insertType, nullptr) == SQLITE_OK;
  ok = ok && sqlite3_prepare_v2(db,
    "INSERT INTO Packets (header, data, timestamp, stream) VALUES (?1, ?2, ?3, ?4);",
    -1, &insertPacket, nullptr) == SQLITE_OK;
  if (!ok) {
    std::cerr << "PacketDatabase: cannot initialize " << path << ": " << sqlite3_errmsg(db) << std::endl;
    sqlite3_finalize(insertType);
    sqlite3_finalize(insertPacket);
    sqlite3_close(db);
    insertType = insertPacket = nullptr;
    db = nullptr;
  }
}


PacketDatabase::~PacketDatabase() {
  if (!db)
    return;
  commit();
  sqlite3_finalize(insertType);
  sqlite3_finalize(insertPacket);
  sqlite3_close(db);
}


PacketDatabase& PacketDatabase::instance() {
  static PacketDatabase instance;
  return instance;
}


bool PacketDatabase::exec(const char* sql) {
  char* error = nullptr;
  if (sqlite3_exec(db, sql, nullptr, nullptr, &error) != SQLITE_OK) {
    std::cerr << "PacketDatabase: " << error << std::endl;
    sqlite3_free(error);
    return false;
  }
  return true;
}


// Packets are inserted inside batched transactions, close the open one if any
void PacketDatabase::commit() {
  if (pending == 0)
    return;
  exec("COMMIT;");
  pending = 0;
}


void PacketDatabase::addType(uint8_t header, const std::string& name) {
  std::lock_guard<std::mutex> guard(lock);
  if (!db)
    return;
  std::string hex = std::format("0x{:02X}", header);
  sqlite3_bind_text(insertType, 1, hex.c_str(), -1, SQLITE_TRANSIENT);
  sqlite3_bind_text(insertType, 2, name.c_str(), -1, SQLITE_TRANSIENT);
  if (sqlite3_step(insertType) != SQLITE_DONE)
    std::cerr << "PacketDatabase: " << sqlite3_errmsg(db) << std::endl;
  sqlite3_reset(insertType);
}


void PacketDatabase::addPacket(uint8_t header, std::span<const uint8_t> data, uint64_t timestamp, uint32_t stream) {
  std::lock_guard<std::mutex> guard(lock);
  if (!db)
    return;
  if (pending == 0)
    exec("BEGIN;");
  //remember that things are hexadecimal so I can either save it as an int val or keep it as text.
  std::string hexHeader = std::format("0x{:02X}", header);
  std::string hexData = "0x";
  for (const auto& byte : data)
    std::format_to(std::back_inserter(hexData), "{:02X}", byte);
  std::string hexTimestamp = std::format("0x{:016X}", timestamp);
  sqlite3_bind_text(insertPacket, 1, hexHeader.c_str(), -1, SQLITE_TRANSIENT);
  sqlite3_bind_text(insertPacket, 2, hexData.c_str(), -1, SQLITE_TRANSIENT);
  sqlite3_bind_text(insertPacket, 3, hexTimestamp.c_str(), -1, SQLITE_TRANSIENT);
  sqlite3_bind_int64(insertPacket, 4, stream);
  if (sqlite3_step(insertPacket) != SQLITE_DONE)
    std::cerr << "PacketDatabase: " << sqlite3_errmsg(db) << std::endl;
  sqlite3_reset(insertPacket);
  if (++pending >= batchSize)
    commit();
}


void PacketDatabase::flush() {
  std::lock_guard<std::mutex> guard(lock);
  if (db)
    commit();
}
