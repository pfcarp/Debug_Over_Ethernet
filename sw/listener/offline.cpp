#include <iostream>
#include <fstream>
#include <filesystem>
#include <string>
#include <vector>
#include <cstdint>
#include <chrono>
#include <ctime>
#include <iomanip>


#include "Sniffer.hpp"


std::string getCurrentTimestamp() {
  // Get current time
  auto now = std::chrono::system_clock::now();
  std::time_t now_time = std::chrono::system_clock::to_time_t(now);
  // Convert to local time
  std::tm local_time = *std::localtime(&now_time);
  // Format as a string (e.g., "2026-05-02-14-30-45")
  std::ostringstream oss;
  oss << std::put_time(&local_time, "%Y-%m-%d-%H-%M-%S");
  return oss.str();
}


int main() {
  std::vector<uint8_t> buffer = std::vector<uint8_t>();
  Sniffer sniffer(&buffer);

  // Ask source
  auto devices = sniffer.getDevices();
  size_t index;
  std::cout << "Select interface to sniff:" << std::endl;
  for (size_t i = 0; i < devices.size(); i++) {
    std::cout << "\t" << i << ". " << devices[i] << std::endl;
  }
  std::cout << "Enter index: " << std::flush;
  std::cin >> index;
  //// Check if the index is valid
  if (index >= devices.size()) {
    std::cout << "Invalid index! Please enter a value between 0 and " << devices.size()-1 << "." << std::endl;
    exit(EXIT_FAILURE);
  }

  // Ask if the user is ready
  std::string response;
  std::cout << "Are you ready? (yes/y): ";
  std::cin >> response;
  // Convert response to lowercase for case-insensitive comparison
  if (response.empty()) {
    std::cout << "No input provided. Exiting." << std::endl;
    return 1;
  }
  char firstChar = tolower(response[0]);
  // Set device and start recording
  if (firstChar == 'y') {
    sniffer.pickDevice(devices[index]);
    std::cout << "Recording of " << devices[index] << " started." << std::endl;
    std::cout << "Press Enter to stop recording..." << std::flush;
    std::cin.ignore(); // Clear the newline left by std::cin
    std::cin.get();    // Wait for Enter key
  } else {
    std::cout << "You did not confirm. Exiting." << std::endl;
    exit(EXIT_FAILURE);
  }

  // Stop recording
  sniffer.unpickDevice();
  //// Create the directory (and parent directories if needed)
  try {
    std::filesystem::create_directories("traces/");
  }
  catch (const std::filesystem::filesystem_error& e) {
    std::cerr << "Filesystem error: " << e.what() << std::endl;
    exit(EXIT_FAILURE);
  }
  //// Dump data
  std::string filename = "traces/"+getCurrentTimestamp()+".bin";
  std::cout << "Trace saved at " << filename << std::endl;
  ////// Open the file in binary mode
  std::ofstream out_file(filename, std::ios::binary);
  if (!out_file) {
    throw std::runtime_error("Failed to open file for writing.");
  }
  ////// Write header
  std::vector<std::string> metadata = {"DOETH", "TPIU ", "v0.1", "00000000"};
  for (const auto& meta : metadata)
    out_file.write(reinterpret_cast<const char*>(meta.data()), meta.size());
  ////// Write the entire buffer as binary data
  out_file.write(reinterpret_cast<const char*>(buffer.data()), buffer.size());
  out_file.close();

  return 0;
}

