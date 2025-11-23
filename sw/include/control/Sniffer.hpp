#pragma once


#include <cstdint>
#include <pcap.h>
#include <string>
#include <vector>


#include "Deformatter.hpp"


class Sniffer {

  private:
    // Attributes
    std::string name;
    pcap_t* interface = nullptr;
    char errbuf[PCAP_ERRBUF_SIZE];
    Deformatter deformatter;
    std::vector<uint8_t> recording;
    // Methods
    void onPacket(const pcap_pkthdr* header, const u_char* packet);
    static void dispatch(u_char* user, const pcap_pkthdr* header, const u_char* packet);
    bool hasHeader(const u_char* packet) const;
    bool hasFooter(const u_char* packet) const;

  public:
    // Attributes
    // Methods
    std::vector<std::string> getDevices();
    void pickDevice(std::string interface);
    void unpickDevice();
    ~Sniffer();

};
