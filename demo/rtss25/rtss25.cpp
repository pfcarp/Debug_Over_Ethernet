#include <cstdint>
#include <iostream>
#include <fstream>
#include <cstdlib>

#include <opencv2/opencv.hpp>
#include "tensorflow/lite/micro/micro_interpreter.h"
#include "tensorflow/lite/micro/system_setup.h"
#include "model_settings.h"
#include "person_detect_model_data.h"
#include "gen_micro_mutable_op_resolver.h"

constexpr int kTensorArenaSize = 1024 * 1024;

class Pipeline {

private:
  // Tensorflow
  const tflite::Model *model = nullptr;
  tflite::MicroInterpreter *interpreter = nullptr;
  TfLiteTensor *input = nullptr;
  int in_h, in_w, in_c;
  uint8_t tensor_arena[kTensorArenaSize];
  uint64_t image_size;
  // Create VideoCapture with a backend (optional but safer)
  cv::VideoCapture cap;
  // Frames
  cv::Mat frame;
  cv::Mat filtered;
  // Control
  bool human = false;
  std::vector<uchar> compressed;
  // Filters
  void sepia();
  void sobel();
  void threshold();
  void duplicate();

public:
  Pipeline(int index);
  bool capture();
  void filter(int mode);
  bool detect();
  void compress();
  void store();
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
  // Tensorflow
  tflite::InitializeTarget();
  model = tflite::GetModel(person_detect_tflite);
  if (model->version() != TFLITE_SCHEMA_VERSION) {
    std::cerr << "Model version mismatch!" << std::endl;
    return;
  }
  std::cout << "Model version: " << model->version() << std::endl;
  static auto op_resolver = get_resolver();
  static tflite::MicroInterpreter static_interpreter(
      model, op_resolver, tensor_arena, kTensorArenaSize);
  interpreter = &static_interpreter;
  TfLiteStatus allocate_status = interpreter->AllocateTensors();
  if (allocate_status != kTfLiteOk) {
    std::cerr << "AllocateTensors() failed" << std::endl;
    return;
  }
  std::cout << "Arena used: " << interpreter->arena_used_bytes() << " bytes" << std::endl;
  input = interpreter->input(0);
  in_h = input->dims->data[1];
  in_w = input->dims->data[2];
  in_c = input->dims->data[3];
  std::cout << "Input type: " << input->type << std::endl;
  std::cout << "Input dims: " << input->dims->data[0] << ", " << input->dims->data[1] << ", " << input->dims->data[2] << ", " << input->dims->data[3] << std::endl;
  // OPS
  auto opcodes = model->operator_codes();
  for (int i = 0; i < opcodes->Length(); i++) {
    auto opcode = opcodes->Get(i);
    auto custom = opcode->custom_code();
    if (custom)
      std::cout << "CUSTOM OP: " << custom->str() << std::endl;
    else
      std::cout << "CUSTOM OP (no name)" << std::endl;
  }
}

bool Pipeline::capture() {
  cap.read(frame);
  return frame.empty();
}

void Pipeline::sepia() {
  cv::Mat kernel = (cv::Mat_<float>(3, 3) << 0.272, 0.534, 0.131, 0.349, 0.686, 0.168, 0.393, 0.769, 0.189);
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
  frame.copyTo(filtered);
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

bool Pipeline::detect() {
  // Preprocess: resize + RGB
  cv::Mat gray, resized, normalized, int8_image;
  cv::cvtColor(frame, gray, cv::COLOR_BGR2GRAY);
  cv::resize(gray, resized, cv::Size(in_w, in_h));
  //omitting weird normalization for int8 model
  // Transfer
  memcpy(input->data.int8, resized.data, in_w * in_h);
  // Inference
  if (input->bytes != in_w * in_h) {
    printf("Wrong image size: expected %d, got %zu", input->bytes, in_w * in_h);
    return false;
  }
  if (kTfLiteOk != interpreter->Invoke()) {
    std::cerr << "Inference failed." << std::endl;
    return false;
  }
  // Get output
  TfLiteTensor *output = interpreter->output(0);
  // uint8_t person_score = output->data.uint8[kPersonIndex];
  human = output->data.uint8[kPersonIndex] < 128;
  return human;
}

void Pipeline::compress() {
  // JPEG compression level: 0 = max compression, 100 = max quality
  std::vector<int> params = { cv::IMWRITE_JPEG_QUALITY, 80 };
  cv::imencode(".jpg", filtered, compressed, params);
}

void Pipeline::store() {
  std::ofstream out("/tmp/dump.jpeg", std::ios::binary);
  if (!out)
    std::cerr << "Error: cannot open /tmp/dup.jpeg for writing\n";
  out.write(reinterpret_cast<const char*>(compressed.data()), compressed.size());
  out.close();
}

void Pipeline::display() {
  if (filtered.empty()) {
    std::cerr << "No frame to display!" << std::endl;
    return;
  }
  // Color frame
  cv::Scalar color = (human)? cv::Scalar(0,255,0) : cv::Scalar(0,0,255);
  int thickness = 8;  // border thickness in pixels
  cv::rectangle(frame, cv::Point(0,0), cv::Point(frame.cols-1, frame.rows-1), color, thickness);
  // Display frame
  cv::imshow("Webcam Feed", frame);
}

void Pipeline::release() {
  cap.release();
}

int main(int argc, char **argv)
{

  int camIndex = 0;
  if (argc > 1) {
    camIndex = std::atoi(argv[1]);
  }

  int mode = 0;
  std::cout << "Press '1' Sepia, '2' Sobel, '3' Threshold, '0' None, 'q' Quit\n";

  Pipeline pipeline(camIndex);

  while (true)
  {

    char key = (char)cv::waitKey(1);
    if (key == 'q')
      break;
    else if (key == '1')
      mode = 1;
    else if (key == '2')
      mode = 2;
    else if (key == '3')
      mode = 3;
    else if (key == '0')
      mode = 0;

    if (pipeline.capture()) {
      break;
    }
    if (pipeline.detect()) {
      pipeline.filter(mode);
      pipeline.compress();
      pipeline.store();
    }
    pipeline.display();
  }

  cv::destroyAllWindows();

  return 0;
}
