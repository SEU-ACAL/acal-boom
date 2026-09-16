// SQLite DPI runtime for boom.perf.ChiselDB.
#include <sqlite3.h>
#include <cctype>
#include <cstdio>
#include <cstdint>
#include <cstdlib>
#include <cstring>
#include <map>
#include <mutex>
#include <sstream>
#include <string>
#include <vector>

namespace {
sqlite3* db = nullptr;
std::string output_path;
std::mutex db_lock;
struct Table {
  std::string schema;
  std::vector<unsigned> widths;
  sqlite3_stmt* insert = nullptr;
};
std::map<std::string, Table> tables;

[[noreturn]] void fail(const std::string& message) {
  std::fprintf(stderr, "ChiselDB: %s\n", message.c_str());
  std::abort();
}
void check(int rc) {
  if (rc != SQLITE_OK) fail(sqlite3_errmsg(db));
}
void exec(const std::string& sql) {
  check(sqlite3_exec(db, sql.c_str(), nullptr, nullptr, nullptr));
}
std::string identifier(const std::string& name) {
  if (name.empty()) fail("empty identifier");
  for (unsigned char c : name)
    if (!std::isalnum(c) && c != '_') fail("invalid identifier: " + name);
  return "\"" + name + "\"";
}
Table& get_table(const char* name, const char* schema) {
  auto found = tables.find(name);
  if (found != tables.end()) {
    if (found->second.schema != schema) fail("schema mismatch for " + std::string(name));
    return found->second;
  }
  Table table;
  table.schema = schema;
  std::string create = "CREATE TABLE " + identifier(name) + "(ID INTEGER PRIMARY KEY AUTOINCREMENT";
  std::string insert = "INSERT INTO " + identifier(name) + " VALUES(NULL";
  std::istringstream fields(schema);
  std::string field;
  while (std::getline(fields, field, ',')) {
    const auto colon = field.find(':');
    if (colon == std::string::npos) fail("invalid schema");
    char* end = nullptr;
    const unsigned long width = std::strtoul(field.c_str() + colon + 1, &end, 10);
    if (*end || width == 0 || width > 64) fail("invalid field width");
    table.widths.push_back(static_cast<unsigned>(width));
    create += "," + identifier(field.substr(0, colon)) + " INTEGER NOT NULL";
    insert += ",?";
  }
  if (table.widths.empty()) fail("empty schema");
  exec(create + ",STAMP INTEGER NOT NULL,SITE TEXT NOT NULL)");
  insert += ",?,?)";
  check(sqlite3_prepare_v2(db, insert.c_str(), -1, &table.insert, nullptr));
  return tables.emplace(name, std::move(table)).first->second;
}
// Extract a <=64-bit column from the hexadecimal MSB-first wire payload.
uint64_t extract(const std::string& payload, unsigned low, unsigned width) {
  if (static_cast<size_t>(low + width) > payload.size() * 4) fail("short payload");
  uint64_t value = 0;
  for (unsigned i = 0; i < width; ++i) {
    const unsigned bit = low + i;
    const unsigned char c = payload[payload.size() - 1 - bit / 4];
    const int digit = c >= '0' && c <= '9' ? c - '0' :
                      c >= 'a' && c <= 'f' ? c - 'a' + 10 :
                      c >= 'A' && c <= 'F' ? c - 'A' + 10 : -1;
    if (digit < 0) fail("unknown (X/Z) or invalid payload bits");
    value |= uint64_t((digit >> (bit % 4)) & 1) << i;
  }
  return value;
}
}
extern "C" void chisel_db_enable(const char* path) {
  std::lock_guard<std::mutex> guard(db_lock);
  if (!path || !path[0]) fail("empty output path");
  if (db) {
    if (output_path != path) fail("writers requested different output paths");
    return;
  }
  check(sqlite3_open(":memory:", &db));
  output_path = path;
  exec("BEGIN TRANSACTION");
}
extern "C" void chisel_db_write(const char* name, const char* schema,
                                uint64_t stamp, const char* site, const char* payload) {
  std::lock_guard<std::mutex> guard(db_lock);
  if (!db) return;
  auto& table = get_table(name, schema);
  unsigned remaining = 0;
  for (unsigned width : table.widths) remaining += width;
  const std::string hex(payload);
  int column = 1;
  for (unsigned width : table.widths) {
    remaining -= width;
    const uint64_t bits = extract(hex, remaining, width);
    sqlite3_int64 signed_bits;
    static_assert(sizeof(signed_bits) == sizeof(bits), "SQLite integer must be 64 bits");
    std::memcpy(&signed_bits, &bits, sizeof(bits));
    check(sqlite3_bind_int64(table.insert, column++, signed_bits));
  }
  check(sqlite3_bind_int64(table.insert, column++, static_cast<sqlite3_int64>(stamp)));
  check(sqlite3_bind_text(table.insert, column, site, -1, SQLITE_TRANSIENT));
  if (sqlite3_step(table.insert) != SQLITE_DONE) fail(sqlite3_errmsg(db));
  check(sqlite3_reset(table.insert));
}
extern "C" void chisel_db_save() {
  std::lock_guard<std::mutex> guard(db_lock);
  if (!db) return; // Multiple writer final blocks share a single database.
  for (auto& table : tables) check(sqlite3_finalize(table.second.insert));
  tables.clear();
  exec("COMMIT");
  sqlite3* disk = nullptr;
  if (sqlite3_open(output_path.c_str(), &disk) != SQLITE_OK) fail("cannot open " + output_path);
  sqlite3_backup* backup = sqlite3_backup_init(disk, "main", db, "main");
  if (!backup) fail(sqlite3_errmsg(disk));
  const int step = sqlite3_backup_step(backup, -1);
  const int finish = sqlite3_backup_finish(backup);
  if (step != SQLITE_DONE || finish != SQLITE_OK) fail(sqlite3_errmsg(disk));
  if (sqlite3_close(disk) != SQLITE_OK) fail("cannot close output database");
  check(sqlite3_close(db));
  db = nullptr;
}
