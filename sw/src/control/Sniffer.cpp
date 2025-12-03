#include "Sniffer.hpp"


#include <cstddef>
#include <cstdint>
#include <iostream>
#include <fstream>
#include <format>


Sniffer::Sniffer(Deformatter& deformatter): deformatter(deformatter) {
  recording.resize(1024*1024);
}

inline bool Sniffer::hasHeader(const u_char* packet) const {
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
  // printf("Packet length: %u\n", header->len);
  if (header->len > 10+headerOffset) {
    if (hasHeader(&packet[headerOffset]) && hasFooter(&packet[header->len-8])) {
      // From here, jump 8 byte by 8 byte (lower half: data, upper half: zeroes)
      // printf("Valid header and footer found\n");
      for (size_t i = 2+headerOffset; i < header->len-8; i += 8) {
        // If next four byte do not compose 0xffffffff, do not skip
        if (!areNext8BytesAllSet(&packet[i])) {
          // printf("1\n");
          // Check if timestamp packet index
          if (goodput%3==0) {
          // if (false) {
            // printf("2\n");
            uint64_t relative = static_cast<uint64_t>(packet[i]);
            for (int j = 1; j < 8; j++)
              relative |= static_cast<uint64_t>(packet[i+j]) << 8*j;
            timestamp += relative;
            // printf("Setting timestamp to %llu\n", timestamp);
            deformatter.setTimestamp(timestamp);
          }
          else {
            // printf("3\n");
            for (int j = 0; j < 8; j++) {
              deformatter.insert(packet[i+j]);
              // recording.push_back(packet[i+j]);
            }
          }
          // Increment goodput counter
          goodput++;
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
  interface = pcap_create(name.c_str(), errbuf);
  if (interface == nullptr) {
    std::cerr << "pcap_open_live failed: " << errbuf << std::endl;
  }
  pcap_set_snaplen(interface, 65535);
  pcap_set_promisc(interface, 1);
  pcap_set_timeout(interface, 10);
  pcap_set_buffer_size(interface, 1024*1024*1024);
  pcap_activate(interface);

  // Start sniffing
  pcap_loop(interface, 0, Sniffer::dispatch, (u_char*)this);
}

void Sniffer::unpickDevice() {
  if (interface != nullptr) {
    pcap_breakloop(interface);
    printStats();
    pcap_close(interface);
    interface = nullptr;
  }
}

void Sniffer::printStats() {
    if (interface != nullptr) {
        struct pcap_stat stats;
        
        if (pcap_stats(interface, &stats) == 0) {
            std::cout << "\n=== Packet Capture Statistics ===" << std::endl;
            std::cout << "Packets received: " << stats.ps_recv << std::endl;
            std::cout << "Packets dropped by kernel: " << stats.ps_drop << std::endl;
            std::cout << "Packets dropped by interface: " << stats.ps_ifdrop << std::endl;
            
            // Calculate drop percentage if any packets were received
            if (stats.ps_recv > 0) {
                double dropRate = (static_cast<double>(stats.ps_drop) / stats.ps_recv) * 100.0;
                std::cout << "Drop rate: " << std::format("{:.2f}%", dropRate) << std::endl;
            }
        } else {
            std::cerr << "Error getting statistics: " << pcap_geterr(interface) << std::endl;
        }
    } else {
        std::cerr << "No active interface to get statistics from" << std::endl;
    }
}

Sniffer::~Sniffer() {
  // Close pcap
  if (interface != nullptr) {
    pcap_breakloop(interface);
    printStats();
    pcap_close(interface);
    interface = nullptr;
  }
  // Dump recording
  std::ofstream out("dump.bin", std::ios::binary);
  std::cout<<""<<std::endl;
  if (!out) {
    throw std::runtime_error("Failed to open file: dump.bin");
  }
  out.write(reinterpret_cast<const char*>(recording.data()), recording.size());
}
