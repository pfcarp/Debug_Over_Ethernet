#include <opencv2/opencv.hpp>
#include <iostream>
#include <chrono>
#include <cstdlib>


class Pipeline {

  private:
    // Create VideoCapture with a backend (optional but safer)
    cv::VideoCapture cap;
    // Frames
    cv::Mat frame;
    cv::Mat filtered;
    // Filters
    void sepia();
    void sobel();
    void threshold();
    void duplicate();
  
  public:
    Pipeline(int index);
    bool capture();
    void filter(int mode);
    void display();
    void release();

};

Pipeline::Pipeline(int index) {
  // Create a resizable window
  cv::namedWindow("Webcam Feed", cv::WINDOW_NORMAL);
  // Create VideoCapture with a backend (optional but safer)
  cap = cv::VideoCapture(index);
  if (!cap.isOpened()) {
    // TODO: switch to throw
    std::cerr << "Error: Could not open camera." << std::endl;
  }
}

bool Pipeline::capture() {
  cap >> frame;
  return frame.empty();
}

void Pipeline::sepia() {
  cv::Mat kernel = (cv::Mat_<float>(3,3) << 0.272, 0.534, 0.131, 0.349, 0.686, 0.168, 0.393, 0.769, 0.189);
  cv::transform(frame, filtered, kernel);
  cv::convertScaleAbs(filtered, filtered);
}

void Pipeline::sobel() {
  cv::Mat gray, gx, gy;
  cv::cvtColor(frame, gray, cv::COLOR_BGR2GRAY);
  cv::Sobel(gray, gx, CV_16S, 1, 0);
  cv::Sobel(gray, gy, CV_16S, 0, 1);
  cv::convertScaleAbs(gx, gx);
  cv::convertScaleAbs(gy, gy);
  cv::addWeighted(gx, 0.5, gy, 0.5, 0, filtered);
  cv::cvtColor(filtered, filtered, cv::COLOR_GRAY2BGR);
}

void Pipeline::threshold() {
  cv::Mat gray;
  cv::cvtColor(frame, gray, cv::COLOR_BGR2GRAY);
  cv::threshold(gray, filtered, 100, 255, cv::THRESH_BINARY);
  cv::cvtColor(filtered, filtered, cv::COLOR_GRAY2BGR);
}

void Pipeline::duplicate() {
  filtered = frame.clone();
}

void Pipeline::filter(int mode) {
  switch (mode) {
    case 1:
      sepia();
      break;
    case 2:
      sobel();
      break;
    case 3:
      threshold();
      break;
    default:
      duplicate();
  }
}

void Pipeline::display() {
  cv::imshow("Webcam Feed", filtered);
}

void Pipeline::release() {
  cap.release();
}


int main(int argc, char** argv) {
    
  int camIndex = 0;
  if (argc > 1) {
    camIndex = std::atoi(argv[1]);
  }

  int mode = 0;
  std::cout << "Press '1' Sepia, '2' Sobel, '3' Threshold, '0' None, 'q' Quit\n";

  Pipeline pipeline(camIndex);

  while (true) {
    
    char key = (char)cv::waitKey(1);
    if (key == 'q') break;
    else if (key == '1') mode = 1;
    else if (key == '2') mode = 2;
    else if (key == '3') mode = 3;
    else if (key == '0') mode = 0;
    
    if (pipeline.capture()) {
      break;
    }
    //if (pipeline.detect()) {
      pipeline.filter(mode);
      //pipeline.compress();
      //pipeline.store();
    //}
    pipeline.display();
  }

  cv::destroyAllWindows();

  return 0;
}

