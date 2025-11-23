#include "Sniffer.hpp"


#include <cstddef>
#include <iostream>
#include <fstream>
#include <format>


inline bool Sniffer::hasHeader(const u_char* packet) const {
  std::cout<<std::format("0x{:02X}",packet[0])<<" "<<std::format("0x{:02X}",packet[1])<<std::endl;
  return (packet[0] == 0xab) && (packet[1] == 0xba);
}

inline bool Sniffer::hasFooter(const u_char* packet) const {
  return (packet[0] == 0xeb) && (packet[1] == 0xbe) &&
    (packet[2] == 0x00) && (packet[3] == 0x00) &&
    (packet[4] == 0x00) && (packet[5] == 0x00) &&
    (packet[6] == 0x00) && (packet[7] == 0x00);
}

inline bool Sniffer::areNext8BytesAllSet(const u_char* packet) const {
  return (packet[0] == 0xff) && (packet[1] == 0xff) && (packet[2] == 0xff) && (packet[3] == 0xff) &&
         (packet[4] == 0xff) && (packet[5] == 0xff) && (packet[6] == 0xff) && (packet[7] == 0xff);
}

void Sniffer::onPacket(const pcap_pkthdr* header, const u_char* packet) {
  // Must be larger than 2+8 (i.e., 0xABBA+0xEBBE000000000000)
  for(int i=0; i<header->len;i++){
    std::cout<<std::format("0x{:02X} ",packet[i]);
  }
  std::cout<<std::endl;
  
  if (header->len > 10+headerOffset) {
    if (hasHeader(&packet[headerOffset]) && hasFooter(&packet[header->len-8])) {
      // From here, jump 8 byte by 8 byte (lower half: data, upper half: zeroes)
      for (size_t i = 2+headerOffset; i < header->len-8; i += 8) {
        // If next four byte do not compose 0xffffffff, do not skip
        if (!areNext8BytesAllSet(&packet[i])) {
          for (int j = 0; j < 4; j++) {
            deformatter.insert(packet[i+j]);
            recording.push_back(packet[i+j]);
          }
        }
      }
    }
    else {
      std::cerr << "Expected header and/or footer (0xABBA, 0xEBBE) not found..." << std::endl;
    }
  }
}

void Sniffer::dispatch(u_char* user, const pcap_pkthdr* header, const u_char* packet) {
  Sniffer* self = reinterpret_cast<Sniffer*>(user);
  self->onPacket(header, packet);
}

std::vector<std::string> Sniffer::getDevices() {
  // Interfaces
  std::vector<std::string> interfaces;
  // PCAP
  pcap_if_t *alldevs, *dev;

  if (pcap_findalldevs(&alldevs, errbuf) == -1) {
    std::cerr << "Error: " << errbuf << std::endl;
  }

  for (dev = alldevs; dev != NULL; dev = dev->next) {
    interfaces.push_back(std::string(dev->name));
  }

  pcap_freealldevs(alldevs);

  return interfaces;
}

void Sniffer::pickDevice(std::string newInterfaceName) {
  // Close current interface is one is already opened
  if (interface != nullptr) {
     pcap_close(interface);
  }
  // Update name
  name = newInterfaceName;
  // Init. interface
  interface = pcap_open_live(name.c_str(), 65535, 1, 10, errbuf);
  if (interface == nullptr) {
    std::cerr << "pcap_open_live failed: " << errbuf << std::endl;
  }
  // Start sniffing
  pcap_loop(interface, 0, Sniffer::dispatch, (u_char*)this);
}

void Sniffer::unpickDevice() {
  if (interface != nullptr) {
    pcap_breakloop(interface);
    pcap_close(interface);
    interface = nullptr;
  }
}

Sniffer::~Sniffer() {
  // Close pcap
  if (interface != nullptr) {
    pcap_breakloop(interface);
    pcap_close(interface);
    interface = nullptr;
  }
  // Dump recording
  std::ofstream out("dump.bin", std::ios::binary);
  if (!out) {
    throw std::runtime_error("Failed to open file: dump.bin");
  }
  out.write(reinterpret_cast<const char*>(recording.data()), recording.size());
}
