#pragma once


#include <string>
#include <vector>


class Sniffer {

  private:
    // Attributes
    std::string interfaceName;
    // Methods

  public:
    // Attributes
    // Methods
    std::vector<std::string> getDevices();
    void setDevice(std::string interface);

};
