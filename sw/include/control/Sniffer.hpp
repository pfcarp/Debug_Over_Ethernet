#pragma once


#include <pcap.h>
#include <string>
#include <vector>


class Sniffer {

  private:
    // Attributes
    std::string name;
    pcap_t* interface = nullptr;
    char errbuf[PCAP_ERRBUF_SIZE];
    // Methods
    void onPacket(const pcap_pkthdr* header, const u_char* packet);
    static void dispatch(u_char* user, const pcap_pkthdr* header, const u_char* packet);

  public:
    // Attributes
    // Methods
    std::vector<std::string> getDevices();
    void pickDevice(std::string interface);
    void unpickDevice();
    ~Sniffer();

};
